#ifndef WGPU_HELPERS_H
#define WGPU_HELPERS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "webgpu.h"
#include "wgpu.h"

/* Opaque context holding everything wgpu needs to run compute. */
typedef struct WGPUContext {
    WGPUInstance instance;
    WGPUAdapter  adapter;
    WGPUDevice   device;
    WGPUQueue    queue;
} WGPUContext;

/* ------------------------------------------------------------------ */
/* Error tracking                                                      */
/* ------------------------------------------------------------------ */

static char wgpu_error_buf[512] = "no error";

static void wgpu_set_error(const char* msg) {
    strncpy(wgpu_error_buf, msg, sizeof(wgpu_error_buf) - 1);
    wgpu_error_buf[sizeof(wgpu_error_buf) - 1] = '\0';
}

static char* wgpu_last_error(void) { return wgpu_error_buf; }

static int wgpu_pipeline_is_null(WGPUComputePipeline p)  { return p  == NULL; }
static int wgpu_buffer_is_null(WGPUBuffer b)              { return b  == NULL; }
static int wgpu_bindgroup_is_null(WGPUBindGroup bg)       { return bg == NULL; }

/* ------------------------------------------------------------------ */
/* Internal callback state                                             */
/* ------------------------------------------------------------------ */

typedef struct { WGPUAdapter adapter; int done; } AdapterResult;
typedef struct { WGPUDevice  device;  int done; } DeviceResult;
typedef struct {                       int done; } MapResult;

static void on_adapter(WGPURequestAdapterStatus status,
                        WGPUAdapter adapter,
                        WGPUStringView message,
                        void* userdata1, void* userdata2) {
    (void)message; (void)userdata2;
    AdapterResult* r = (AdapterResult*)userdata1;
    r->adapter = (status == WGPURequestAdapterStatus_Success) ? adapter : NULL;
    if (!r->adapter) fprintf(stderr, "wgpu: adapter request failed\n");
    r->done = 1;
}

static void on_device(WGPURequestDeviceStatus status,
                       WGPUDevice device,
                       WGPUStringView message,
                       void* userdata1, void* userdata2) {
    (void)message; (void)userdata2;
    DeviceResult* r = (DeviceResult*)userdata1;
    r->device = (status == WGPURequestDeviceStatus_Success) ? device : NULL;
    if (!r->device) fprintf(stderr, "wgpu: device request failed\n");
    r->done = 1;
}

static void on_device_error(WGPUDevice const* device,
                             WGPUErrorType type,
                             WGPUStringView message,
                             void* userdata1, void* userdata2) {
    (void)device; (void)type; (void)userdata1; (void)userdata2;
    if (message.data && message.length > 0)
        wgpu_set_error(message.data);
}

static void on_map(WGPUMapAsyncStatus status,
                   WGPUStringView message,
                   void* userdata1, void* userdata2) {
    (void)status; (void)message; (void)userdata2;
    ((MapResult*)userdata1)->done = 1;
}

/* ------------------------------------------------------------------ */
/* Context create / free                                               */
/* ------------------------------------------------------------------ */

static WGPUContext* wgpu_context_create(void) {
    WGPUContext* ctx = calloc(1, sizeof(WGPUContext));

    ctx->instance = wgpuCreateInstance(NULL);
    if (!ctx->instance) {
        wgpu_set_error("wgpuCreateInstance failed — no Vulkan/Metal/DX12 backend available");
        free(ctx); return NULL;
    }

    AdapterResult ar = {0};
    WGPURequestAdapterCallbackInfo adapter_cb = {
        .mode = WGPUCallbackMode_AllowSpontaneous, .callback = on_adapter, .userdata1 = &ar,
    };
    wgpuInstanceRequestAdapter(ctx->instance, NULL, adapter_cb);
    while (!ar.done) wgpuInstanceProcessEvents(ctx->instance);

    if (!ar.adapter) {
        wgpu_set_error("wgpuInstanceRequestAdapter failed — no suitable GPU adapter found");
        wgpuInstanceRelease(ctx->instance);
        free(ctx); return NULL;
    }
    ctx->adapter = ar.adapter;

    DeviceResult dr = {0};
    WGPURequestDeviceCallbackInfo device_cb = {
        .mode = WGPUCallbackMode_AllowSpontaneous, .callback = on_device, .userdata1 = &dr,
    };
    WGPUDeviceDescriptor device_desc = {
        .uncapturedErrorCallbackInfo = {
            .callback  = on_device_error,
        },
    };
    wgpuAdapterRequestDevice(ctx->adapter, &device_desc, device_cb);
    while (!dr.done) wgpuInstanceProcessEvents(ctx->instance);

    if (!dr.device) {
        wgpu_set_error("wgpuAdapterRequestDevice failed — device creation refused");
        wgpuAdapterRelease(ctx->adapter);
        wgpuInstanceRelease(ctx->instance);
        free(ctx); return NULL;
    }
    ctx->device = dr.device;
    ctx->queue  = wgpuDeviceGetQueue(ctx->device);
    return ctx;
}

static void wgpu_context_free(WGPUContext* ctx) {
    if (!ctx) return;
    wgpuQueueRelease(ctx->queue);
    wgpuDeviceRelease(ctx->device);
    wgpuAdapterRelease(ctx->adapter);
    wgpuInstanceRelease(ctx->instance);
    free(ctx);
}

/* ------------------------------------------------------------------ */
/* Compute pipeline                                                    */
/* ------------------------------------------------------------------ */

static WGPUComputePipeline wgpu_create_compute_pipeline(WGPUContext* ctx,
                                                          const char* wgsl_source,
                                                          const char* entry_point) {
    WGPUShaderSourceWGSL wgsl = {
        .chain = { .sType = WGPUSType_ShaderSourceWGSL },
        .code  = { .data = wgsl_source, .length = strlen(wgsl_source) },
    };
    WGPUShaderModuleDescriptor shader_desc = { .nextInChain = &wgsl.chain };
    WGPUShaderModule shader = wgpuDeviceCreateShaderModule(ctx->device, &shader_desc);
    if (!shader) {
        wgpu_set_error("wgpuDeviceCreateShaderModule failed — check WGSL source for syntax errors");
        return NULL;
    }

    WGPUComputePipelineDescriptor pipeline_desc = {
        .compute = {
            .module     = shader,
            .entryPoint = { .data = entry_point, .length = strlen(entry_point) },
        },
    };
    WGPUComputePipeline pipeline = wgpuDeviceCreateComputePipeline(ctx->device, &pipeline_desc);
    wgpuShaderModuleRelease(shader);
    if (!pipeline)
        wgpu_set_error("wgpuDeviceCreateComputePipeline failed — check entry point name matches shader");
    return pipeline;
}

/* ------------------------------------------------------------------ */
/* Buffers                                                             */
/* ------------------------------------------------------------------ */

static WGPUBuffer wgpu_create_storage_buffer(WGPUContext* ctx, uint64_t size) {
    WGPUBufferDescriptor desc = {
        .usage            = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst | WGPUBufferUsage_CopySrc,
        .size             = size,
        .mappedAtCreation = 0,
    };
    return wgpuDeviceCreateBuffer(ctx->device, &desc);
}

static void wgpu_write_buffer(WGPUContext* ctx, WGPUBuffer buffer,
                               const void* data, uint64_t size) {
    wgpuQueueWriteBuffer(ctx->queue, buffer, 0, data, size);
}

/* ------------------------------------------------------------------ */
/* Bind group                                                          */
/* ------------------------------------------------------------------ */

static WGPUBindGroup wgpu_create_bind_group(WGPUContext* ctx,
                                             WGPUComputePipeline pipeline,
                                             WGPUBuffer* buffers,
                                             uint32_t buffer_count) {
    WGPUBindGroupLayout layout = wgpuComputePipelineGetBindGroupLayout(pipeline, 0);

    WGPUBindGroupEntry entries[8];
    uint32_t count = buffer_count < 8 ? buffer_count : 8;
    for (uint32_t i = 0; i < count; i++) {
        entries[i] = (WGPUBindGroupEntry){
            .binding = i, .buffer = buffers[i], .offset = 0, .size = WGPU_WHOLE_SIZE,
        };
    }
    WGPUBindGroupDescriptor desc = {
        .layout = layout, .entryCount = count, .entries = entries,
    };
    WGPUBindGroup bg = wgpuDeviceCreateBindGroup(ctx->device, &desc);
    wgpuBindGroupLayoutRelease(layout);
    return bg;
}

/* ------------------------------------------------------------------ */
/* Dispatch                                                            */
/* ------------------------------------------------------------------ */

static void wgpu_dispatch_and_wait(WGPUContext* ctx,
                                    WGPUComputePipeline pipeline,
                                    WGPUBindGroup bind_group,
                                    uint32_t workgroup_count_x) {
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
    WGPUComputePassEncoder pass = wgpuCommandEncoderBeginComputePass(encoder, NULL);
    wgpuComputePassEncoderSetPipeline(pass, pipeline);
    wgpuComputePassEncoderSetBindGroup(pass, 0, bind_group, 0, NULL);
    wgpuComputePassEncoderDispatchWorkgroups(pass, workgroup_count_x, 1, 1);
    wgpuComputePassEncoderEnd(pass);
    wgpuComputePassEncoderRelease(pass);

    WGPUCommandBuffer cmd = wgpuCommandEncoderFinish(encoder, NULL);
    wgpuCommandEncoderRelease(encoder);
    wgpuQueueSubmit(ctx->queue, 1, &cmd);
    wgpuCommandBufferRelease(cmd);
    wgpuDevicePoll(ctx->device, 1, NULL);
}

/* ------------------------------------------------------------------ */
/* Readback                                                            */
/* ------------------------------------------------------------------ */

static void wgpu_read_buffer(WGPUContext* ctx, WGPUBuffer buffer,
                              void* out, uint64_t size) {
    WGPUBufferDescriptor staging_desc = {
        .usage = WGPUBufferUsage_CopyDst | WGPUBufferUsage_MapRead,
        .size  = size, .mappedAtCreation = 0,
    };
    WGPUBuffer staging = wgpuDeviceCreateBuffer(ctx->device, &staging_desc);

    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
    wgpuCommandEncoderCopyBufferToBuffer(encoder, buffer, 0, staging, 0, size);
    WGPUCommandBuffer cmd = wgpuCommandEncoderFinish(encoder, NULL);
    wgpuCommandEncoderRelease(encoder);
    wgpuQueueSubmit(ctx->queue, 1, &cmd);
    wgpuCommandBufferRelease(cmd);
    wgpuDevicePoll(ctx->device, 1, NULL);

    MapResult mr = {0};
    WGPUBufferMapCallbackInfo map_cb = {
        .mode = WGPUCallbackMode_AllowSpontaneous, .callback = on_map, .userdata1 = &mr,
    };
    wgpuBufferMapAsync(staging, WGPUMapMode_Read, 0, size, map_cb);
    while (!mr.done) wgpuInstanceProcessEvents(ctx->instance);

    const void* mapped = wgpuBufferGetMappedRange(staging, 0, size);
    memcpy(out, mapped, size);
    wgpuBufferUnmap(staging);
    wgpuBufferRelease(staging);
}

#endif /* WGPU_HELPERS_H */
