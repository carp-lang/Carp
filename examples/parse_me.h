// This is a comment
void boo();
char hoo();
// This is also a comment
int foo(int x);
float goo(double x, double y);

// some people use too much whitespace
  float  weird  ( double  x ,  double  y  )  ;

// Struct types
vector2 middle(vector2 p1, vector2 p2);

// Pointers are also useful to parse
void gah(int *x);
void dah(int *x, float* y);
char *blah(int *x, float* y);

vector2 *more(vector3* in);

GLFWAPI GLFWwindow* glfwCreateWindow(int width, int height, const char* title, GLFWmonitor* monitor, GLFWwindow* share);


// TODO:
GLFWAPI void glfwPollEvents(void);



// C Macros

#define K_BLAH 12345
#define K_BLAX 0x123
#define K_GLAH 12345

// C function-like macros

#define X(a, b) a + b
#define Y(a, b) {\\
  foo(a, b);\\
}
#define Z(a, b) "hi"
