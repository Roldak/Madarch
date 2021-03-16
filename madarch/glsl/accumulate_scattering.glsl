#include "maths.glsl"
#include "scene.glsl"
#include "raymarching.glsl"
#include "cook_torrance_brdf.glsl"
#include "volumetrics.glsl"

layout(binding = 2) uniform sampler2D visibility_data;

vec3 sample_visibility(float len, vec2 norm_pos) {
   float rel_pos_depth = floor(len * 10.0);
   float coord_x = norm_pos.x;
   float coord_y = (norm_pos.y + rel_pos_depth) / 60.0;
   vec2 tex_coord = vec2(coord_x, coord_y);
   return texture2D(visibility_data, tex_coord).rgb;
}

vec4 accumulate_scattering(vec3 from, vec3 dir, vec2 norm_pos) {
   vec3 to = from + 6.0 * dir;
   int prim_index;
   raycast(from, dir, prim_index, to);
   float len = min(length(to - from), 6.0);
   vec3 L = vec3(0);
   for (float f = 0.0; f < len; f += light_shafts_step_length) {
      vec3 r = sample_visibility(f, norm_pos);
      L += r * exp(-f * tau_scattering);
   }
   L *= light_shafts_step_length;
   return vec4(L, len);
}

in vec4 pos;
out vec4 color;

uniform vec3 camera_position;
uniform mat3 camera_orientation;

void main(void) {
   vec3 frag_pos = vec3(pos.xy, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   dir = camera_orientation * dir;
   vec3 initial_pos = camera_orientation * frag_pos + camera_position;

   color = accumulate_scattering(
      initial_pos,
      dir,
      0.5 * (frag_pos.xy + vec2(1))
   );
}
