#version 120

#include "raymarch.glsl"

varying vec4 pos;
uniform vec3 camera_position;

void main(void) {
   vec3 frag_pos = vec3(pos.xy, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   vec3 light_pos = vec3(cos(time), 2, sin(time));
   vec3 initial_pos = frag_pos + camera_position;

   vec3 col = pixel_color_direct(initial_pos, dir, light_pos);
   //gl_FragColor = vec4(pow(col / (col + vec3(1.0)), vec3(0.4545)), 1);
   gl_FragColor = texture2D(irradiance_data, (frag_pos.xy + vec2(1)) * 0.5);
}
