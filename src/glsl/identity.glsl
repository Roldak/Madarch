#version 120

varying vec4 pos;

void main(void)
{
   vec4 v = ftransform();
   gl_Position = v;
   pos = gl_Vertex;
   gl_FrontColor = gl_Color;
}
