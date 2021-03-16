#include "maths.glsl"
#include "probe_utils.glsl"
#include "scene.glsl"
#include "raymarching.glsl"
#include "cook_torrance_brdf.glsl"
#include "volumetrics.glsl"

vec3 sample_lights(vec3 pos, vec3 dir) {
   vec3 result = vec3(0);
   for (int i = 0; i < total_light_count; ++i) {
      vec3 L;
      float L_dist;
      vec3 radiance = sample_light(i, pos, vec3(1, 0, 0), L, L_dist);
      float visibility = raycast_visibility(pos, L, L_dist);
      vec3 L_in = exp(-L_dist * tau_scattering) * visibility * radiance;
      result += L_in * tau_scattering * henvey_greenstein_phase(L, dir);
   }
   return result;
}

in vec4 pos;
out vec3 color;

uniform vec3 camera_position;
uniform mat3 camera_orientation;

void main(void) {
   float norm_height = (pos.y + 1.0) * 0.5;
   float tex_height = norm_height * 60;
   float depth = floor(tex_height);
   float fract_height = tex_height - depth;
   float frag_height = fract_height * 2.0 - 1.0;
   vec3 frag_pos = vec3(pos.x, frag_height, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   dir = camera_orientation * dir;
   frag_pos = camera_orientation * frag_pos;
   vec3 initial_pos = frag_pos + camera_position;
   color = sample_lights(initial_pos + dir * depth * 0.1, dir);
}
