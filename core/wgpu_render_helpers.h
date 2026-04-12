#ifndef WGPU_RENDER_HELPERS_H
#define WGPU_RENDER_HELPERS_H

/* These headers define many static helper functions; not all will be used
 * in every compilation unit.  Suppress the warning so -Werror doesn't fire. */
#pragma clang diagnostic ignored "-Wunused-function"

#include "wgpu_helpers.h"

/* ------------------------------------------------------------------ */
/* Wrapper structs (names chosen to avoid clashing with wgpu typedefs) */
/* ------------------------------------------------------------------ */

typedef struct WGPURenderSurface {
    WGPUSurface   surface;
    WGPUTextureFormat format;
    uint32_t width;
    uint32_t height;
} WGPURenderSurface;

typedef struct WGPURenderTexture {
    WGPUTexture     texture;
    WGPUTextureView view;
    WGPUTextureFormat format;
    uint32_t width;
    uint32_t height;
} WGPURenderTexture;

typedef struct WGPURenderPipelineWrapper {
    WGPURenderPipeline   pipeline;
    WGPUPipelineLayout   layout;
    WGPUBindGroupLayout  bgl;
} WGPURenderPipelineWrapper;

typedef struct WGPUFrameState {
    WGPUCommandEncoder  encoder;
    WGPUTextureView     swapchain_view;
    WGPUSurfaceTexture  surface_texture;
} WGPUFrameState;

/* ------------------------------------------------------------------ */
/* Null checks                                                         */
/* ------------------------------------------------------------------ */

static int wgpu_render_surface_is_null(WGPURenderSurface* s)        { return s == NULL; }
static int wgpu_render_texture_is_null(WGPURenderTexture* rt)       { return rt == NULL; }
static int wgpu_render_pipeline_is_null(WGPURenderPipelineWrapper* p) { return p == NULL; }
static int wgpu_frame_is_null(WGPUFrameState* f)                    { return f == NULL; }
static int wgpu_sampler_is_null(WGPUSampler s)                      { return s == NULL; }

/* ------------------------------------------------------------------ */
/* Texture format parsing                                              */
/* ------------------------------------------------------------------ */

static WGPUTextureFormat wgpu_parse_texture_format(const char* name) {
    if (!name) return WGPUTextureFormat_Undefined;
    if (strcmp(name, "bgra8unorm")  == 0) return WGPUTextureFormat_BGRA8Unorm;
    if (strcmp(name, "rgba8unorm")  == 0) return WGPUTextureFormat_RGBA8Unorm;
    if (strcmp(name, "rgba16float") == 0) return WGPUTextureFormat_RGBA16Float;
    if (strcmp(name, "bgra8unorm-srgb") == 0) return WGPUTextureFormat_BGRA8UnormSrgb;
    if (strcmp(name, "rgba8unorm-srgb") == 0) return WGPUTextureFormat_RGBA8UnormSrgb;
    return WGPUTextureFormat_Undefined;
}

/* ------------------------------------------------------------------ */
/* Surface & Swapchain                                                 */
/* ------------------------------------------------------------------ */

static WGPURenderSurface* wgpu_create_surface(WGPUContext* ctx,
                                               void* native_window_handle,
                                               void* native_display,
                                               uint32_t width,
                                               uint32_t height) {
    if (!ctx || !ctx->instance) {
        wgpu_set_error("wgpu_create_surface: null context or instance");
        return NULL;
    }

    /* Build platform-specific surface source.
     * On Linux we support X11 (via Xlib) and Wayland.
     * native_display == NULL is treated as X11 with the default display. */
    WGPUSurface surface = NULL;

#if defined(__linux__) || defined(__unix__)
    if (native_display != NULL) {
        /* Try X11 Xlib first (most common on Linux desktops).
         * native_window_handle is the Window (uint32_t cast to void*).
         * native_display is the Display*. */
        WGPUSurfaceSourceXlibWindow x11_source = {
            .chain = { .sType = WGPUSType_SurfaceSourceXlibWindow },
            .display = native_display,
            .window  = (uint64_t)(uintptr_t)native_window_handle,
        };
        WGPUSurfaceDescriptor surf_desc = {
            .nextInChain = &x11_source.chain,
        };
        surface = wgpuInstanceCreateSurface(ctx->instance, &surf_desc);
    } else {
        /* Wayland fallback: native_window_handle is wl_surface*,
         * native_display would be wl_display* but we use NULL for X11 default. */
        wgpu_set_error("wgpu_create_surface: native_display is NULL — provide X11 Display* or Wayland wl_display*");
        return NULL;
    }
#elif defined(__APPLE__)
    /* macOS / iOS: native_window_handle is a CAMetalLayer*. */
    WGPUSurfaceSourceMetalLayer metal_source = {
        .chain = { .sType = WGPUSType_SurfaceSourceMetalLayer },
        .layer = native_window_handle,
    };
    WGPUSurfaceDescriptor surf_desc = {
        .nextInChain = &metal_source.chain,
    };
    surface = wgpuInstanceCreateSurface(ctx->instance, &surf_desc);
    (void)native_display;
#elif defined(_WIN32)
    /* Windows: native_window_handle is HWND, native_display is HINSTANCE. */
    WGPUSurfaceSourceWindowsHWND win_source = {
        .chain = { .sType = WGPUSType_SurfaceSourceWindowsHWND },
        .hinstance = native_display,
        .hwnd      = native_window_handle,
    };
    WGPUSurfaceDescriptor surf_desc = {
        .nextInChain = &win_source.chain,
    };
    surface = wgpuInstanceCreateSurface(ctx->instance, &surf_desc);
#else
    wgpu_set_error("wgpu_create_surface: unsupported platform");
    return NULL;
#endif

    if (!surface) {
        wgpu_set_error("wgpuInstanceCreateSurface failed — could not create surface from native handles");
        return NULL;
    }

    /* Query preferred format from the surface capabilities. */
    WGPUSurfaceCapabilities caps = {0};
    wgpuSurfaceGetCapabilities(surface, ctx->adapter, &caps);

    WGPUTextureFormat format = WGPUTextureFormat_BGRA8Unorm; /* sensible default */
    if (caps.formatCount > 0 && caps.formats) {
        format = caps.formats[0]; /* use the surface's preferred format */
    }
    wgpuSurfaceCapabilitiesFreeMembers(caps);

    /* Configure the surface. */
    WGPUSurfaceConfiguration config = {
        .device      = ctx->device,
        .format      = format,
        .usage       = WGPUTextureUsage_RenderAttachment,
        .alphaMode   = WGPUCompositeAlphaMode_Opaque,
        .width       = width,
        .height      = height,
        .presentMode = WGPUPresentMode_Fifo,
    };
    wgpuSurfaceConfigure(surface, &config);

    WGPURenderSurface* rs = calloc(1, sizeof(WGPURenderSurface));
    if (!rs) {
        wgpu_set_error("wgpu_create_surface: allocation failed");
        wgpuSurfaceRelease(surface);
        return NULL;
    }
    rs->surface = surface;
    rs->format  = format;
    rs->width   = width;
    rs->height  = height;
    return rs;
}

/* Returns the surface's texture format as a format string suitable for
 * passing back to wgpu_create_render_pipeline_str / wgpu_create_geom_pipeline_str.
 * Useful when the GPU selects a different format than the hardcoded default
 * (e.g. Bgra8UnormSrgb instead of Bgra8Unorm). */
static char* wgpu_surface_format_str(WGPURenderSurface* surf) {
    if (!surf) return "bgra8unorm";
    switch (surf->format) {
        case WGPUTextureFormat_BGRA8Unorm:     return "bgra8unorm";
        case WGPUTextureFormat_BGRA8UnormSrgb: return "bgra8unorm-srgb";
        case WGPUTextureFormat_RGBA8Unorm:     return "rgba8unorm";
        case WGPUTextureFormat_RGBA8UnormSrgb: return "rgba8unorm-srgb";
        case WGPUTextureFormat_RGBA16Float:    return "rgba16float";
        default:                               return "bgra8unorm";
    }
}

static void wgpu_surface_free(WGPURenderSurface* surf) {
    if (!surf) return;
    wgpuSurfaceUnconfigure(surf->surface);
    wgpuSurfaceRelease(surf->surface);
    free(surf);
}

static void wgpu_surface_resize(WGPURenderSurface* surf, WGPUContext* ctx,
                                 uint32_t width, uint32_t height) {
    if (!surf || !ctx) return;
    if (width == 0 || height == 0) return; /* degenerate resize, skip */

    WGPUSurfaceConfiguration config = {
        .device      = ctx->device,
        .format      = surf->format,
        .usage       = WGPUTextureUsage_RenderAttachment,
        .alphaMode   = WGPUCompositeAlphaMode_Opaque,
        .width       = width,
        .height      = height,
        .presentMode = WGPUPresentMode_Fifo,
    };
    wgpuSurfaceConfigure(surf->surface, &config);
    surf->width  = width;
    surf->height = height;
}

/* ------------------------------------------------------------------ */
/* Textures & render targets                                           */
/* ------------------------------------------------------------------ */

static WGPURenderTexture* wgpu_create_render_texture(WGPUContext* ctx,
                                                      uint32_t width,
                                                      uint32_t height,
                                                      WGPUTextureFormat format) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_render_texture: null context");
        return NULL;
    }
    if (format == WGPUTextureFormat_Undefined) {
        wgpu_set_error("wgpu_create_render_texture: undefined texture format");
        return NULL;
    }

    WGPUTextureDescriptor tex_desc = {
        .usage       = WGPUTextureUsage_RenderAttachment | WGPUTextureUsage_TextureBinding,
        .dimension   = WGPUTextureDimension_2D,
        .size        = { .width = width, .height = height, .depthOrArrayLayers = 1 },
        .format      = format,
        .mipLevelCount   = 1,
        .sampleCount     = 1,
    };
    WGPUTexture texture = wgpuDeviceCreateTexture(ctx->device, &tex_desc);
    if (!texture) {
        wgpu_set_error("wgpuDeviceCreateTexture failed for render texture");
        return NULL;
    }

    WGPUTextureViewDescriptor view_desc = {
        .format          = format,
        .dimension       = WGPUTextureViewDimension_2D,
        .baseMipLevel    = 0,
        .mipLevelCount   = 1,
        .baseArrayLayer  = 0,
        .arrayLayerCount = 1,
        .aspect          = WGPUTextureAspect_All,
    };
    WGPUTextureView view = wgpuTextureCreateView(texture, &view_desc);
    if (!view) {
        wgpu_set_error("wgpuTextureCreateView failed for render texture");
        wgpuTextureRelease(texture);
        return NULL;
    }

    WGPURenderTexture* rt = calloc(1, sizeof(WGPURenderTexture));
    if (!rt) {
        wgpu_set_error("wgpu_create_render_texture: allocation failed");
        wgpuTextureViewRelease(view);
        wgpuTextureRelease(texture);
        return NULL;
    }
    rt->texture = texture;
    rt->view    = view;
    rt->format  = format;
    rt->width   = width;
    rt->height  = height;
    return rt;
}

static void wgpu_render_texture_free(WGPURenderTexture* rt) {
    if (!rt) return;
    wgpuTextureViewRelease(rt->view);
    wgpuTextureRelease(rt->texture);
    free(rt);
}

static WGPUSampler wgpu_create_sampler(WGPUContext* ctx) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_sampler: null context");
        return NULL;
    }
    WGPUSamplerDescriptor desc = {
        .addressModeU  = WGPUAddressMode_ClampToEdge,
        .addressModeV  = WGPUAddressMode_ClampToEdge,
        .addressModeW  = WGPUAddressMode_ClampToEdge,
        .magFilter     = WGPUFilterMode_Linear,
        .minFilter     = WGPUFilterMode_Linear,
        .mipmapFilter  = WGPUMipmapFilterMode_Linear,
        .lodMinClamp   = 0.0f,
        .lodMaxClamp   = 32.0f,
        .compare       = WGPUCompareFunction_Undefined,
        .maxAnisotropy = 1,
    };
    WGPUSampler sampler = wgpuDeviceCreateSampler(ctx->device, &desc);
    if (!sampler) {
        wgpu_set_error("wgpuDeviceCreateSampler failed");
    }
    return sampler;
}

/* ------------------------------------------------------------------ */
/* Render pipeline                                                     */
/* ------------------------------------------------------------------ */

static WGPURenderPipelineWrapper* wgpu_create_render_pipeline(WGPUContext* ctx,
                                                                const char* wgsl_source,
                                                                const char* vs_entry,
                                                                const char* fs_entry,
                                                                WGPUTextureFormat target_format) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_render_pipeline: null context");
        return NULL;
    }
    if (target_format == WGPUTextureFormat_Undefined) {
        wgpu_set_error("wgpu_create_render_pipeline: undefined target format");
        return NULL;
    }

    /* Compile the shader module. */
    WGPUShaderSourceWGSL wgsl = {
        .chain = { .sType = WGPUSType_ShaderSourceWGSL },
        .code  = { .data = wgsl_source, .length = strlen(wgsl_source) },
    };
    WGPUShaderModuleDescriptor shader_desc = { .nextInChain = &wgsl.chain };
    WGPUShaderModule shader = wgpuDeviceCreateShaderModule(ctx->device, &shader_desc);
    if (!shader) {
        wgpu_set_error("wgpu_create_render_pipeline: shader compilation failed — check WGSL source");
        return NULL;
    }

    /* Color target state. */
    WGPUColorTargetState color_target = {
        .format    = target_format,
        .writeMask = WGPUColorWriteMask_All,
    };

    /* Fragment state. */
    WGPUFragmentState fragment = {
        .module      = shader,
        .entryPoint  = { .data = fs_entry, .length = strlen(fs_entry) },
        .targetCount = 1,
        .targets     = &color_target,
    };

    /* Render pipeline descriptor — no vertex buffers (fullscreen tri from vertex index). */
    WGPURenderPipelineDescriptor pipe_desc = {
        .vertex = {
            .module      = shader,
            .entryPoint  = { .data = vs_entry, .length = strlen(vs_entry) },
            .bufferCount = 0,
            .buffers     = NULL,
        },
        .primitive = {
            .topology         = WGPUPrimitiveTopology_TriangleList,
            .stripIndexFormat = WGPUIndexFormat_Undefined,
            .frontFace        = WGPUFrontFace_CCW,
            .cullMode         = WGPUCullMode_None,
        },
        .multisample = {
            .count = 1,
            .mask  = ~(uint32_t)0,
            .alphaToCoverageEnabled = 0,
        },
        .fragment = &fragment,
    };

    WGPURenderPipeline pipeline = wgpuDeviceCreateRenderPipeline(ctx->device, &pipe_desc);
    wgpuShaderModuleRelease(shader);
    if (!pipeline) {
        wgpu_set_error("wgpu_create_render_pipeline: pipeline creation failed — check entry points");
        return NULL;
    }

    /* Extract the bind group layout at index 0 for later bind group creation. */
    WGPUBindGroupLayout bgl = wgpuRenderPipelineGetBindGroupLayout(pipeline, 0);

    WGPURenderPipelineWrapper* wrap = calloc(1, sizeof(WGPURenderPipelineWrapper));
    if (!wrap) {
        wgpu_set_error("wgpu_create_render_pipeline: allocation failed");
        if (bgl) wgpuBindGroupLayoutRelease(bgl);
        wgpuRenderPipelineRelease(pipeline);
        return NULL;
    }
    wrap->pipeline = pipeline;
    wrap->layout   = NULL; /* auto layout — no explicit pipeline layout */
    wrap->bgl      = bgl;
    return wrap;
}

static void wgpu_render_pipeline_free(WGPURenderPipelineWrapper* pipe) {
    if (!pipe) return;
    if (pipe->bgl)      wgpuBindGroupLayoutRelease(pipe->bgl);
    if (pipe->layout)   wgpuPipelineLayoutRelease(pipe->layout);
    if (pipe->pipeline) wgpuRenderPipelineRelease(pipe->pipeline);
    free(pipe);
}

/* ------------------------------------------------------------------ */
/* Bind groups for rendering                                           */
/* ------------------------------------------------------------------ */

static WGPUBindGroup wgpu_create_texture_bind_group(WGPUContext* ctx,
                                                     WGPURenderPipelineWrapper* pipe,
                                                     WGPURenderTexture* texture,
                                                     WGPUSampler sampler) {
    if (!ctx || !pipe || !texture || !sampler) {
        wgpu_set_error("wgpu_create_texture_bind_group: null argument");
        return NULL;
    }
    if (!pipe->bgl) {
        wgpu_set_error("wgpu_create_texture_bind_group: pipeline has no bind group layout");
        return NULL;
    }

    WGPUBindGroupEntry entries[2] = {
        {
            .binding     = 0,
            .textureView = texture->view,
        },
        {
            .binding = 1,
            .sampler = sampler,
        },
    };

    WGPUBindGroupDescriptor desc = {
        .layout     = pipe->bgl,
        .entryCount = 2,
        .entries    = entries,
    };

    WGPUBindGroup bg = wgpuDeviceCreateBindGroup(ctx->device, &desc);
    if (!bg) {
        wgpu_set_error("wgpu_create_texture_bind_group: bind group creation failed");
    }
    return bg;
}

/* ------------------------------------------------------------------ */
/* Pass execution                                                      */
/* ------------------------------------------------------------------ */

static WGPUFrameState* wgpu_begin_frame(WGPURenderSurface* surf, WGPUContext* ctx) {
    if (!surf || !ctx) {
        wgpu_set_error("wgpu_begin_frame: null surface or context");
        return NULL;
    }

    WGPUFrameState* frame = calloc(1, sizeof(WGPUFrameState));
    if (!frame) {
        wgpu_set_error("wgpu_begin_frame: allocation failed");
        return NULL;
    }

    wgpuSurfaceGetCurrentTexture(surf->surface, &frame->surface_texture);
    if ((frame->surface_texture.status != WGPUSurfaceGetCurrentTextureStatus_SuccessOptimal &&
         frame->surface_texture.status != WGPUSurfaceGetCurrentTextureStatus_SuccessSuboptimal) ||
        !frame->surface_texture.texture) {
        wgpu_set_error("wgpu_begin_frame: failed to acquire swapchain texture");
        free(frame);
        return NULL;
    }

    WGPUTextureViewDescriptor view_desc = {
        .format          = surf->format,
        .dimension       = WGPUTextureViewDimension_2D,
        .baseMipLevel    = 0,
        .mipLevelCount   = 1,
        .baseArrayLayer  = 0,
        .arrayLayerCount = 1,
        .aspect          = WGPUTextureAspect_All,
    };
    frame->swapchain_view = wgpuTextureCreateView(frame->surface_texture.texture, &view_desc);
    if (!frame->swapchain_view) {
        wgpu_set_error("wgpu_begin_frame: failed to create swapchain texture view");
        /* surface_texture.texture is owned by the surface — do not release it */
        free(frame);
        return NULL;
    }

    frame->encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
    if (!frame->encoder) {
        wgpu_set_error("wgpu_begin_frame: failed to create command encoder");
        wgpuTextureViewRelease(frame->swapchain_view);
        /* surface_texture.texture is owned by the surface — do not release it */
        free(frame);
        return NULL;
    }

    return frame;
}

static void wgpu_run_pass(WGPUFrameState* frame,
                           WGPURenderPipelineWrapper* pipe,
                           WGPUBindGroup bind_group,
                           WGPURenderTexture* target) {
    if (!frame || !pipe) return;

    /* Choose render target: offscreen texture if provided, else swapchain. */
    WGPUTextureView target_view = target ? target->view : frame->swapchain_view;

    WGPURenderPassColorAttachment color_att = {
        .view       = target_view,
        .loadOp     = WGPULoadOp_Clear,
        .storeOp    = WGPUStoreOp_Store,
        .clearValue = { .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 },
        .depthSlice = WGPU_DEPTH_SLICE_UNDEFINED,
    };

    WGPURenderPassDescriptor pass_desc = {
        .colorAttachmentCount = 1,
        .colorAttachments     = &color_att,
    };

    WGPURenderPassEncoder pass = wgpuCommandEncoderBeginRenderPass(frame->encoder, &pass_desc);
    if (!pass) return;

    wgpuRenderPassEncoderSetPipeline(pass, pipe->pipeline);
    if (bind_group) {
        wgpuRenderPassEncoderSetBindGroup(pass, 0, bind_group, 0, NULL);
    }
    /* Draw 3 vertices — fullscreen triangle generated from vertex index in shader. */
    wgpuRenderPassEncoderDraw(pass, 3, 1, 0, 0);
    wgpuRenderPassEncoderEnd(pass);
    wgpuRenderPassEncoderRelease(pass);
}

static void wgpu_end_frame(WGPUFrameState* frame, WGPURenderSurface* surf, WGPUContext* ctx) {
    if (!frame || !surf || !ctx) return;

    WGPUCommandBuffer cmd = wgpuCommandEncoderFinish(frame->encoder, NULL);
    wgpuCommandEncoderRelease(frame->encoder);
    frame->encoder = NULL; /* consumed — prevent double-release in frame_free */

    if (cmd) {
        wgpuQueueSubmit(ctx->queue, 1, &cmd);
        wgpuCommandBufferRelease(cmd);
    }

    wgpuSurfacePresent(surf->surface);
    wgpuDevicePoll(ctx->device, 0, NULL);
}

static void wgpu_frame_free(WGPUFrameState* frame) {
    if (!frame) return;
    if (frame->encoder)        wgpuCommandEncoderRelease(frame->encoder);
    if (frame->swapchain_view) wgpuTextureViewRelease(frame->swapchain_view);
    /* Note: surface_texture.texture is owned by the surface/swapchain.
     * We must NOT release it — only the view we created from it. */
    free(frame);
}

/* ------------------------------------------------------------------ */
/* Format-string variants for Carp interop                             */
/* ------------------------------------------------------------------ */

static WGPURenderTexture* wgpu_create_render_texture_str(WGPUContext* ctx,
                                                          uint32_t width,
                                                          uint32_t height,
                                                          const char* format_str) {
    WGPUTextureFormat fmt = wgpu_parse_texture_format(format_str);
    if (fmt == WGPUTextureFormat_Undefined) {
        char buf[256];
        snprintf(buf, sizeof(buf),
                 "wgpu_create_render_texture: unknown format \"%s\" — use bgra8unorm, rgba8unorm, or rgba16float",
                 format_str ? format_str : "(null)");
        wgpu_set_error(buf);
        return NULL;
    }
    return wgpu_create_render_texture(ctx, width, height, fmt);
}

static WGPURenderPipelineWrapper* wgpu_create_render_pipeline_str(WGPUContext* ctx,
                                                                    const char* wgsl_source,
                                                                    const char* vs_entry,
                                                                    const char* fs_entry,
                                                                    const char* format_str) {
    WGPUTextureFormat fmt = wgpu_parse_texture_format(format_str);
    if (fmt == WGPUTextureFormat_Undefined) {
        char buf[256];
        snprintf(buf, sizeof(buf),
                 "wgpu_create_render_pipeline: unknown format \"%s\" — use bgra8unorm, rgba8unorm, or rgba16float",
                 format_str ? format_str : "(null)");
        wgpu_set_error(buf);
        return NULL;
    }
    return wgpu_create_render_pipeline(ctx, wgsl_source, vs_entry, fs_entry, fmt);
}

/* ------------------------------------------------------------------ */
/* GLFW surface bridge                                                 */
/* Creates a WGPURenderSurface from a GLFWwindow* with automatic      */
/* platform detection. Requires GLFW_CLIENT_API / GLFW_NO_API hint    */
/* before window creation so GLFW does not create an OpenGL context.  */
/* ------------------------------------------------------------------ */

#include <GLFW/glfw3.h>

#if defined(__linux__) || defined(__unix__)
#  ifndef GLFW_EXPOSE_NATIVE_X11
#    define GLFW_EXPOSE_NATIVE_X11
#  endif
#  include <GLFW/glfw3native.h>
/* X11/X.h defines `#define Success 0` which collides with Carp's generated
 * `Result_Success` struct tag name.  Undefine it immediately after the include
 * so the rest of the translation unit (and Carp's out/main.c) compiles cleanly. */
#  ifdef Success
#    undef Success
#  endif
#elif defined(_WIN32)
#  ifndef GLFW_EXPOSE_NATIVE_WIN32
#    define GLFW_EXPOSE_NATIVE_WIN32
#  endif
#  include <GLFW/glfw3native.h>
#endif

static WGPURenderSurface* wgpu_create_surface_from_glfw(WGPUContext* ctx,
                                                         GLFWwindow* window,
                                                         int width,
                                                         int height) {
    if (!ctx || !window) {
        wgpu_set_error("wgpu_create_surface_from_glfw: null argument");
        return NULL;
    }

    WGPUSurface surface = NULL;

#if defined(__linux__) || defined(__unix__)
    Display* display = glfwGetX11Display();
    Window   x11win  = glfwGetX11Window(window);
    WGPUSurfaceSourceXlibWindow x11_source = {
        .chain   = { .sType = WGPUSType_SurfaceSourceXlibWindow },
        .display = display,
        .window  = (uint64_t)x11win,
    };
    WGPUSurfaceDescriptor surf_desc = { .nextInChain = &x11_source.chain };
    surface = wgpuInstanceCreateSurface(ctx->instance, &surf_desc);
#elif defined(_WIN32)
    HWND      hwnd      = glfwGetWin32Window(window);
    HINSTANCE hinstance = GetModuleHandle(NULL);
    WGPUSurfaceSourceWindowsHWND win_source = {
        .chain     = { .sType = WGPUSType_SurfaceSourceWindowsHWND },
        .hinstance = hinstance,
        .hwnd      = hwnd,
    };
    WGPUSurfaceDescriptor surf_desc = { .nextInChain = &win_source.chain };
    surface = wgpuInstanceCreateSurface(ctx->instance, &surf_desc);
#else
    wgpu_set_error("wgpu_create_surface_from_glfw: unsupported platform");
    return NULL;
#endif

    if (!surface) {
        wgpu_set_error("wgpu_create_surface_from_glfw: wgpuInstanceCreateSurface failed");
        return NULL;
    }

    /* Query preferred swapchain format. */
    WGPUSurfaceCapabilities caps = {0};
    wgpuSurfaceGetCapabilities(surface, ctx->adapter, &caps);
    WGPUTextureFormat format = WGPUTextureFormat_BGRA8Unorm;
    if (caps.formatCount > 0 && caps.formats) {
        format = caps.formats[0];
    }
    wgpuSurfaceCapabilitiesFreeMembers(caps);

    WGPUSurfaceConfiguration config = {
        .device      = ctx->device,
        .format      = format,
        .usage       = WGPUTextureUsage_RenderAttachment,
        .alphaMode   = WGPUCompositeAlphaMode_Opaque,
        .width       = (uint32_t)width,
        .height      = (uint32_t)height,
        .presentMode = WGPUPresentMode_Fifo,
    };
    wgpuSurfaceConfigure(surface, &config);

#if defined(__linux__) || defined(__unix__)
    /* On X11 without a compositor, the window background pixel is unset so
     * old screen content bleeds through on expose events.  Set it to black. */
    {
        Display* dpy  = glfwGetX11Display();
        Window   xwin = glfwGetX11Window(window);
        XSetWindowBackground(dpy, xwin, BlackPixel(dpy, DefaultScreen(dpy)));
        XClearWindow(dpy, xwin);
    }
#endif

    WGPURenderSurface* rs = calloc(1, sizeof(WGPURenderSurface));
    if (!rs) {
        wgpu_set_error("wgpu_create_surface_from_glfw: allocation failed");
        wgpuSurfaceRelease(surface);
        return NULL;
    }
    rs->surface = surface;
    rs->format  = format;
    rs->width   = (uint32_t)width;
    rs->height  = (uint32_t)height;
    return rs;
}

/* ================================================================== */
/* Vertex buffer, uniform buffer, depth texture, geometry pipeline     */
/* Index buffer, indexed drawing                                       */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/* Wrapper structs                                                     */
/* ------------------------------------------------------------------ */

typedef struct WGPUVertexBufferWrapper {
    WGPUBuffer buffer;
    uint64_t   size;
} WGPUVertexBufferWrapper;

typedef struct WGPUUniformBufferWrapper {
    WGPUBuffer buffer;
    uint64_t   size;
} WGPUUniformBufferWrapper;

typedef struct WGPUDepthTexture {
    WGPUTexture     texture;
    WGPUTextureView view;
    uint32_t        width;
    uint32_t        height;
} WGPUDepthTexture;

typedef struct WGPUGeomPipelineWrapper {
    WGPURenderPipeline  pipeline;
    WGPUBindGroupLayout bgl;
} WGPUGeomPipelineWrapper;

typedef struct WGPUIndexBufferWrapper {
    WGPUBuffer buffer;
    uint32_t   count;
} WGPUIndexBufferWrapper;

/* ------------------------------------------------------------------ */
/* Null checks for new types                                           */
/* ------------------------------------------------------------------ */

static int wgpu_vertex_buffer_is_null(WGPUVertexBufferWrapper* vb)   { return vb == NULL; }
static int wgpu_uniform_buffer_is_null(WGPUUniformBufferWrapper* ub) { return ub == NULL; }
static int wgpu_depth_texture_is_null(WGPUDepthTexture* dt)          { return dt == NULL; }
static int wgpu_geom_pipeline_is_null(WGPUGeomPipelineWrapper* p)    { return p  == NULL; }
static int wgpu_index_buffer_is_null(WGPUIndexBufferWrapper* ib)     { return ib == NULL; }

/* ------------------------------------------------------------------ */
/* Vertex buffer                                                       */
/* ------------------------------------------------------------------ */

static WGPUVertexBufferWrapper* wgpu_create_vertex_buffer(WGPUContext* ctx,
                                                           const void* data,
                                                           uint64_t size) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_vertex_buffer: null context");
        return NULL;
    }
    if (!data || size == 0) {
        wgpu_set_error("wgpu_create_vertex_buffer: null data or zero size");
        return NULL;
    }

    WGPUBufferDescriptor desc = {
        .usage            = WGPUBufferUsage_Vertex | WGPUBufferUsage_CopyDst,
        .size             = size,
        .mappedAtCreation = 1,
    };
    WGPUBuffer buffer = wgpuDeviceCreateBuffer(ctx->device, &desc);
    if (!buffer) {
        wgpu_set_error("wgpu_create_vertex_buffer: wgpuDeviceCreateBuffer failed");
        return NULL;
    }

    void* mapped = wgpuBufferGetMappedRange(buffer, 0, size);
    if (!mapped) {
        wgpu_set_error("wgpu_create_vertex_buffer: failed to map buffer");
        wgpuBufferRelease(buffer);
        return NULL;
    }
    memcpy(mapped, data, size);
    wgpuBufferUnmap(buffer);

    WGPUVertexBufferWrapper* vb = calloc(1, sizeof(WGPUVertexBufferWrapper));
    if (!vb) {
        wgpu_set_error("wgpu_create_vertex_buffer: allocation failed");
        wgpuBufferRelease(buffer);
        return NULL;
    }
    vb->buffer = buffer;
    vb->size   = size;
    return vb;
}

static void wgpu_vertex_buffer_free(WGPUVertexBufferWrapper* vb) {
    if (!vb) return;
    if (vb->buffer) wgpuBufferRelease(vb->buffer);
    free(vb);
}

/* ------------------------------------------------------------------ */
/* Uniform buffer                                                      */
/* ------------------------------------------------------------------ */

static WGPUUniformBufferWrapper* wgpu_create_uniform_buffer(WGPUContext* ctx,
                                                              uint64_t size) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_uniform_buffer: null context");
        return NULL;
    }
    if (size == 0) {
        wgpu_set_error("wgpu_create_uniform_buffer: zero size");
        return NULL;
    }

    WGPUBufferDescriptor desc = {
        .usage            = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst,
        .size             = size,
        .mappedAtCreation = 0,
    };
    WGPUBuffer buffer = wgpuDeviceCreateBuffer(ctx->device, &desc);
    if (!buffer) {
        wgpu_set_error("wgpu_create_uniform_buffer: wgpuDeviceCreateBuffer failed");
        return NULL;
    }

    WGPUUniformBufferWrapper* ub = calloc(1, sizeof(WGPUUniformBufferWrapper));
    if (!ub) {
        wgpu_set_error("wgpu_create_uniform_buffer: allocation failed");
        wgpuBufferRelease(buffer);
        return NULL;
    }
    ub->buffer = buffer;
    ub->size   = size;
    return ub;
}

static void wgpu_update_uniform_buffer(WGPUContext* ctx,
                                        WGPUUniformBufferWrapper* ub,
                                        const void* data,
                                        uint64_t size) {
    if (!ctx || !ub || !data) return;
    uint64_t write_size = size < ub->size ? size : ub->size;
    wgpuQueueWriteBuffer(ctx->queue, ub->buffer, 0, data, write_size);
}

static void wgpu_uniform_buffer_free(WGPUUniformBufferWrapper* ub) {
    if (!ub) return;
    if (ub->buffer) wgpuBufferRelease(ub->buffer);
    free(ub);
}

/* ------------------------------------------------------------------ */
/* Depth texture                                                       */
/* ------------------------------------------------------------------ */

static WGPUDepthTexture* wgpu_create_depth_texture(WGPUContext* ctx,
                                                     uint32_t width,
                                                     uint32_t height) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_depth_texture: null context");
        return NULL;
    }
    if (width == 0 || height == 0) {
        wgpu_set_error("wgpu_create_depth_texture: zero width or height");
        return NULL;
    }

    WGPUTextureDescriptor tex_desc = {
        .usage           = WGPUTextureUsage_RenderAttachment,
        .dimension       = WGPUTextureDimension_2D,
        .size            = { .width = width, .height = height, .depthOrArrayLayers = 1 },
        .format          = WGPUTextureFormat_Depth24Plus,
        .mipLevelCount   = 1,
        .sampleCount     = 1,
    };
    WGPUTexture texture = wgpuDeviceCreateTexture(ctx->device, &tex_desc);
    if (!texture) {
        wgpu_set_error("wgpu_create_depth_texture: wgpuDeviceCreateTexture failed");
        return NULL;
    }

    WGPUTextureViewDescriptor view_desc = {
        .format          = WGPUTextureFormat_Depth24Plus,
        .dimension       = WGPUTextureViewDimension_2D,
        .baseMipLevel    = 0,
        .mipLevelCount   = 1,
        .baseArrayLayer  = 0,
        .arrayLayerCount = 1,
        .aspect          = WGPUTextureAspect_DepthOnly,
    };
    WGPUTextureView view = wgpuTextureCreateView(texture, &view_desc);
    if (!view) {
        wgpu_set_error("wgpu_create_depth_texture: wgpuTextureCreateView failed");
        wgpuTextureRelease(texture);
        return NULL;
    }

    WGPUDepthTexture* dt = calloc(1, sizeof(WGPUDepthTexture));
    if (!dt) {
        wgpu_set_error("wgpu_create_depth_texture: allocation failed");
        wgpuTextureViewRelease(view);
        wgpuTextureRelease(texture);
        return NULL;
    }
    dt->texture = texture;
    dt->view    = view;
    dt->width   = width;
    dt->height  = height;
    return dt;
}

static void wgpu_depth_texture_free(WGPUDepthTexture* dt) {
    if (!dt) return;
    if (dt->view)    wgpuTextureViewRelease(dt->view);
    if (dt->texture) wgpuTextureRelease(dt->texture);
    free(dt);
}

/* ------------------------------------------------------------------ */
/* Geometry render pipeline (with vertex buffers + depth + uniforms)    */
/* ------------------------------------------------------------------ */

static WGPUGeomPipelineWrapper* wgpu_create_geom_pipeline(WGPUContext* ctx,
                                                            const char* wgsl_source,
                                                            const char* vs_entry,
                                                            const char* fs_entry,
                                                            WGPUTextureFormat target_format) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_geom_pipeline: null context");
        return NULL;
    }
    if (target_format == WGPUTextureFormat_Undefined) {
        wgpu_set_error("wgpu_create_geom_pipeline: undefined target format");
        return NULL;
    }

    /* Compile shader module. */
    WGPUShaderSourceWGSL wgsl = {
        .chain = { .sType = WGPUSType_ShaderSourceWGSL },
        .code  = { .data = wgsl_source, .length = strlen(wgsl_source) },
    };
    WGPUShaderModuleDescriptor shader_desc = { .nextInChain = &wgsl.chain };
    WGPUShaderModule shader = wgpuDeviceCreateShaderModule(ctx->device, &shader_desc);
    if (!shader) {
        wgpu_set_error("wgpu_create_geom_pipeline: shader compilation failed — check WGSL source");
        return NULL;
    }

    /* Bind group layout: one uniform buffer at binding 0, visible to vertex+fragment. */
    WGPUBindGroupLayoutEntry bgl_entry = {
        .binding    = 0,
        .visibility = WGPUShaderStage_Vertex | WGPUShaderStage_Fragment,
        .buffer     = {
            .type           = WGPUBufferBindingType_Uniform,
            .hasDynamicOffset = 0,
            .minBindingSize   = 0,
        },
    };
    WGPUBindGroupLayoutDescriptor bgl_desc = {
        .entryCount = 1,
        .entries    = &bgl_entry,
    };
    WGPUBindGroupLayout bgl = wgpuDeviceCreateBindGroupLayout(ctx->device, &bgl_desc);
    if (!bgl) {
        wgpu_set_error("wgpu_create_geom_pipeline: bind group layout creation failed");
        wgpuShaderModuleRelease(shader);
        return NULL;
    }

    /* Pipeline layout with the one bind group layout. */
    WGPUPipelineLayoutDescriptor pl_desc = {
        .bindGroupLayoutCount = 1,
        .bindGroupLayouts     = &bgl,
    };
    WGPUPipelineLayout pipeline_layout = wgpuDeviceCreatePipelineLayout(ctx->device, &pl_desc);
    if (!pipeline_layout) {
        wgpu_set_error("wgpu_create_geom_pipeline: pipeline layout creation failed");
        wgpuBindGroupLayoutRelease(bgl);
        wgpuShaderModuleRelease(shader);
        return NULL;
    }

    /* Vertex buffer layout: interleaved position (float32x3) + color (float32x3). */
    WGPUVertexAttribute attrs[2] = {
        {
            .format         = WGPUVertexFormat_Float32x3,
            .offset         = 0,
            .shaderLocation = 0,
        },
        {
            .format         = WGPUVertexFormat_Float32x3,
            .offset         = 12,
            .shaderLocation = 1,
        },
    };
    WGPUVertexBufferLayout vb_layout = {
        .arrayStride    = 24,
        .stepMode       = WGPUVertexStepMode_Vertex,
        .attributeCount = 2,
        .attributes     = attrs,
    };

    /* Color target state. */
    WGPUColorTargetState color_target = {
        .format    = target_format,
        .writeMask = WGPUColorWriteMask_All,
    };

    /* Fragment state. */
    WGPUFragmentState fragment = {
        .module      = shader,
        .entryPoint  = { .data = fs_entry, .length = strlen(fs_entry) },
        .targetCount = 1,
        .targets     = &color_target,
    };

    /* Depth/stencil state. */
    WGPUDepthStencilState depth_stencil = {
        .format              = WGPUTextureFormat_Depth24Plus,
        .depthWriteEnabled   = WGPUOptionalBool_True,
        .depthCompare        = WGPUCompareFunction_Less,
        .stencilFront        = {
            .compare         = WGPUCompareFunction_Always,
            .failOp          = WGPUStencilOperation_Keep,
            .depthFailOp     = WGPUStencilOperation_Keep,
            .passOp          = WGPUStencilOperation_Keep,
        },
        .stencilBack         = {
            .compare         = WGPUCompareFunction_Always,
            .failOp          = WGPUStencilOperation_Keep,
            .depthFailOp     = WGPUStencilOperation_Keep,
            .passOp          = WGPUStencilOperation_Keep,
        },
        .stencilReadMask     = 0xFFFFFFFF,
        .stencilWriteMask    = 0xFFFFFFFF,
        .depthBias           = 0,
        .depthBiasSlopeScale = 0.0f,
        .depthBiasClamp      = 0.0f,
    };

    /* Render pipeline descriptor with vertex buffers + depth. */
    WGPURenderPipelineDescriptor pipe_desc = {
        .layout   = pipeline_layout,
        .vertex   = {
            .module      = shader,
            .entryPoint  = { .data = vs_entry, .length = strlen(vs_entry) },
            .bufferCount = 1,
            .buffers     = &vb_layout,
        },
        .primitive = {
            .topology         = WGPUPrimitiveTopology_TriangleList,
            .stripIndexFormat = WGPUIndexFormat_Undefined,
            .frontFace        = WGPUFrontFace_CCW,
            .cullMode         = WGPUCullMode_Back,
        },
        .depthStencil = &depth_stencil,
        .multisample  = {
            .count = 1,
            .mask  = ~(uint32_t)0,
            .alphaToCoverageEnabled = 0,
        },
        .fragment = &fragment,
    };

    WGPURenderPipeline pipeline = wgpuDeviceCreateRenderPipeline(ctx->device, &pipe_desc);
    wgpuPipelineLayoutRelease(pipeline_layout);
    wgpuShaderModuleRelease(shader);

    if (!pipeline) {
        wgpu_set_error("wgpu_create_geom_pipeline: pipeline creation failed — check entry points and vertex layout");
        wgpuBindGroupLayoutRelease(bgl);
        return NULL;
    }

    WGPUGeomPipelineWrapper* wrap = calloc(1, sizeof(WGPUGeomPipelineWrapper));
    if (!wrap) {
        wgpu_set_error("wgpu_create_geom_pipeline: allocation failed");
        wgpuBindGroupLayoutRelease(bgl);
        wgpuRenderPipelineRelease(pipeline);
        return NULL;
    }
    wrap->pipeline = pipeline;
    wrap->bgl      = bgl;
    return wrap;
}

static WGPUGeomPipelineWrapper* wgpu_create_geom_pipeline_str(WGPUContext* ctx,
                                                                const char* wgsl_source,
                                                                const char* vs_entry,
                                                                const char* fs_entry,
                                                                const char* format_str) {
    WGPUTextureFormat fmt = wgpu_parse_texture_format(format_str);
    if (fmt == WGPUTextureFormat_Undefined) {
        char buf[256];
        snprintf(buf, sizeof(buf),
                 "wgpu_create_geom_pipeline: unknown format \"%s\" — use bgra8unorm, rgba8unorm, or rgba16float",
                 format_str ? format_str : "(null)");
        wgpu_set_error(buf);
        return NULL;
    }
    return wgpu_create_geom_pipeline(ctx, wgsl_source, vs_entry, fs_entry, fmt);
}

static void wgpu_geom_pipeline_free(WGPUGeomPipelineWrapper* pipe) {
    if (!pipe) return;
    if (pipe->bgl)      wgpuBindGroupLayoutRelease(pipe->bgl);
    if (pipe->pipeline) wgpuRenderPipelineRelease(pipe->pipeline);
    free(pipe);
}

/* ------------------------------------------------------------------ */
/* Uniform bind group for geometry pipeline                            */
/* ------------------------------------------------------------------ */

static WGPUBindGroup wgpu_create_uniform_bind_group(WGPUContext* ctx,
                                                      WGPUGeomPipelineWrapper* pipe,
                                                      WGPUUniformBufferWrapper* ub) {
    if (!ctx || !pipe || !ub) {
        wgpu_set_error("wgpu_create_uniform_bind_group: null argument");
        return NULL;
    }
    if (!pipe->bgl) {
        wgpu_set_error("wgpu_create_uniform_bind_group: pipeline has no bind group layout");
        return NULL;
    }

    WGPUBindGroupEntry entry = {
        .binding = 0,
        .buffer  = ub->buffer,
        .offset  = 0,
        .size    = ub->size,
    };
    WGPUBindGroupDescriptor desc = {
        .layout     = pipe->bgl,
        .entryCount = 1,
        .entries    = &entry,
    };

    WGPUBindGroup bg = wgpuDeviceCreateBindGroup(ctx->device, &desc);
    if (!bg) {
        wgpu_set_error("wgpu_create_uniform_bind_group: bind group creation failed");
    }
    return bg;
}

/* ------------------------------------------------------------------ */
/* Geometry pass (non-indexed)                                         */
/* ------------------------------------------------------------------ */

static void wgpu_run_geom_pass(WGPUFrameState* frame,
                                WGPUGeomPipelineWrapper* pipe,
                                WGPUBindGroup bind_group,
                                WGPUVertexBufferWrapper* vb,
                                WGPUDepthTexture* depth,
                                uint32_t vertex_count,
                                WGPURenderTexture* target) {
    if (!frame || !pipe || !vb || !depth) return;

    WGPUTextureView target_view = target ? target->view : frame->swapchain_view;

    WGPURenderPassColorAttachment color_att = {
        .view       = target_view,
        .loadOp     = WGPULoadOp_Clear,
        .storeOp    = WGPUStoreOp_Store,
        .clearValue = { .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 },
        .depthSlice = WGPU_DEPTH_SLICE_UNDEFINED,
    };

    WGPURenderPassDepthStencilAttachment depth_att = {
        .view              = depth->view,
        .depthLoadOp       = WGPULoadOp_Clear,
        .depthStoreOp      = WGPUStoreOp_Store,
        .depthClearValue   = 1.0f,
        .stencilLoadOp     = WGPULoadOp_Undefined,
        .stencilStoreOp    = WGPUStoreOp_Undefined,
    };

    WGPURenderPassDescriptor pass_desc = {
        .colorAttachmentCount    = 1,
        .colorAttachments        = &color_att,
        .depthStencilAttachment  = &depth_att,
    };

    WGPURenderPassEncoder pass = wgpuCommandEncoderBeginRenderPass(frame->encoder, &pass_desc);
    if (!pass) return;

    wgpuRenderPassEncoderSetPipeline(pass, pipe->pipeline);
    if (bind_group) {
        wgpuRenderPassEncoderSetBindGroup(pass, 0, bind_group, 0, NULL);
    }
    wgpuRenderPassEncoderSetVertexBuffer(pass, 0, vb->buffer, 0, vb->size);
    wgpuRenderPassEncoderDraw(pass, vertex_count, 1, 0, 0);
    wgpuRenderPassEncoderEnd(pass);
    wgpuRenderPassEncoderRelease(pass);
}

/* ------------------------------------------------------------------ */
/* Index buffer                                                        */
/* ------------------------------------------------------------------ */

static WGPUIndexBufferWrapper* wgpu_create_index_buffer(WGPUContext* ctx,
                                                          const uint32_t* data,
                                                          uint32_t count) {
    if (!ctx) {
        wgpu_set_error("wgpu_create_index_buffer: null context");
        return NULL;
    }
    if (!data || count == 0) {
        wgpu_set_error("wgpu_create_index_buffer: null data or zero count");
        return NULL;
    }

    uint64_t size = (uint64_t)count * sizeof(uint32_t);
    WGPUBufferDescriptor desc = {
        .usage            = WGPUBufferUsage_Index | WGPUBufferUsage_CopyDst,
        .size             = size,
        .mappedAtCreation = 1,
    };
    WGPUBuffer buffer = wgpuDeviceCreateBuffer(ctx->device, &desc);
    if (!buffer) {
        wgpu_set_error("wgpu_create_index_buffer: wgpuDeviceCreateBuffer failed");
        return NULL;
    }

    void* mapped = wgpuBufferGetMappedRange(buffer, 0, size);
    if (!mapped) {
        wgpu_set_error("wgpu_create_index_buffer: failed to map buffer");
        wgpuBufferRelease(buffer);
        return NULL;
    }
    memcpy(mapped, data, size);
    wgpuBufferUnmap(buffer);

    WGPUIndexBufferWrapper* ib = calloc(1, sizeof(WGPUIndexBufferWrapper));
    if (!ib) {
        wgpu_set_error("wgpu_create_index_buffer: allocation failed");
        wgpuBufferRelease(buffer);
        return NULL;
    }
    ib->buffer = buffer;
    ib->count  = count;
    return ib;
}

static void wgpu_index_buffer_free(WGPUIndexBufferWrapper* ib) {
    if (!ib) return;
    if (ib->buffer) wgpuBufferRelease(ib->buffer);
    free(ib);
}

/* ------------------------------------------------------------------ */
/* Geometry pass (indexed)                                             */
/* ------------------------------------------------------------------ */

static void wgpu_run_geom_pass_indexed(WGPUFrameState* frame,
                                         WGPUGeomPipelineWrapper* pipe,
                                         WGPUBindGroup bind_group,
                                         WGPUVertexBufferWrapper* vb,
                                         WGPUIndexBufferWrapper* ib,
                                         WGPUDepthTexture* depth,
                                         WGPURenderTexture* target) {
    if (!frame || !pipe || !vb || !ib || !depth) return;

    WGPUTextureView target_view = target ? target->view : frame->swapchain_view;

    WGPURenderPassColorAttachment color_att = {
        .view       = target_view,
        .loadOp     = WGPULoadOp_Clear,
        .storeOp    = WGPUStoreOp_Store,
        .clearValue = { .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 },
        .depthSlice = WGPU_DEPTH_SLICE_UNDEFINED,
    };

    WGPURenderPassDepthStencilAttachment depth_att = {
        .view              = depth->view,
        .depthLoadOp       = WGPULoadOp_Clear,
        .depthStoreOp      = WGPUStoreOp_Store,
        .depthClearValue   = 1.0f,
        .stencilLoadOp     = WGPULoadOp_Undefined,
        .stencilStoreOp    = WGPUStoreOp_Undefined,
    };

    WGPURenderPassDescriptor pass_desc = {
        .colorAttachmentCount    = 1,
        .colorAttachments        = &color_att,
        .depthStencilAttachment  = &depth_att,
    };

    WGPURenderPassEncoder pass = wgpuCommandEncoderBeginRenderPass(frame->encoder, &pass_desc);
    if (!pass) return;

    wgpuRenderPassEncoderSetPipeline(pass, pipe->pipeline);
    if (bind_group) {
        wgpuRenderPassEncoderSetBindGroup(pass, 0, bind_group, 0, NULL);
    }
    wgpuRenderPassEncoderSetVertexBuffer(pass, 0, vb->buffer, 0, vb->size);
    uint64_t ib_size = (uint64_t)ib->count * sizeof(uint32_t);
    wgpuRenderPassEncoderSetIndexBuffer(pass, ib->buffer, WGPUIndexFormat_Uint32, 0, ib_size);
    wgpuRenderPassEncoderDrawIndexed(pass, ib->count, 1, 0, 0, 0);
    wgpuRenderPassEncoderEnd(pass);
    wgpuRenderPassEncoderRelease(pass);
}

/* ------------------------------------------------------------------ */
/* Bind group: uniform buffer for fullscreen render pipeline           */
/* ------------------------------------------------------------------ */

static WGPUBindGroup wgpu_create_render_uniform_bind_group(
    WGPUContext* ctx,
    WGPURenderPipelineWrapper* pipe,
    WGPUUniformBufferWrapper* ub)
{
    if (!ctx || !pipe || !ub) {
        wgpu_set_error("wgpu_create_render_uniform_bind_group: null argument");
        return NULL;
    }

    WGPUBindGroupLayout layout = wgpuRenderPipelineGetBindGroupLayout(pipe->pipeline, 0);
    if (!layout) {
        wgpu_set_error("wgpu_create_render_uniform_bind_group: failed to get bind group layout");
        return NULL;
    }

    WGPUBindGroupEntry entry = {
        .binding = 0,
        .buffer  = ub->buffer,
        .offset  = 0,
        .size    = ub->size,
    };
    WGPUBindGroupDescriptor desc = {
        .layout     = layout,
        .entryCount = 1,
        .entries    = &entry,
    };

    WGPUBindGroup bg = wgpuDeviceCreateBindGroup(ctx->device, &desc);
    wgpuBindGroupLayoutRelease(layout);
    if (!bg) {
        wgpu_set_error("wgpu_create_render_uniform_bind_group: bind group creation failed");
    }
    return bg;
}

/* ------------------------------------------------------------------ */
/* Bind group: storage buffer + uniform buffer for fullscreen pipeline  */
/* ------------------------------------------------------------------ */

static WGPUBindGroup wgpu_create_render_storage_uniform_bind_group(
    WGPUContext* ctx,
    WGPURenderPipelineWrapper* pipe,
    WGPUBuffer storage_buf,
    WGPUUniformBufferWrapper* ub)
{
    if (!ctx || !pipe || !ub) {
        wgpu_set_error("wgpu_create_render_storage_uniform_bind_group: null argument");
        return NULL;
    }

    WGPUBindGroupLayout layout = wgpuRenderPipelineGetBindGroupLayout(pipe->pipeline, 0);
    if (!layout) {
        wgpu_set_error("wgpu_create_render_storage_uniform_bind_group: failed to get bind group layout");
        return NULL;
    }

    WGPUBindGroupEntry entries[2] = {
        {
            .binding = 0,
            .buffer  = storage_buf,
            .offset  = 0,
            .size    = WGPU_WHOLE_SIZE,
        },
        {
            .binding = 1,
            .buffer  = ub->buffer,
            .offset  = 0,
            .size    = ub->size,
        },
    };
    WGPUBindGroupDescriptor desc = {
        .layout     = layout,
        .entryCount = 2,
        .entries    = entries,
    };

    WGPUBindGroup bg = wgpuDeviceCreateBindGroup(ctx->device, &desc);
    wgpuBindGroupLayoutRelease(layout);
    if (!bg) {
        wgpu_set_error("wgpu_create_render_storage_uniform_bind_group: bind group creation failed");
    }
    return bg;
}

#endif /* WGPU_RENDER_HELPERS_H */
