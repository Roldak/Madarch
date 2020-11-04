uniform float time;

#include "maths.glsl"
#include "probe_utils.glsl"
#include "random.glsl"
#include "scene.glsl"
#include "materials.glsl"
#include "raymarching.glsl"
#include "cook_torrance_brdf.glsl"
#include "lighting.glsl"
#include "render_probes.glsl"

in vec4 pos;
out vec3 color;

uniform vec3 camera_position;

void main(void) {
   vec3 frag_pos = vec3(pos.xy, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   vec3 initial_pos = frag_pos + camera_position;

   //float sa = rand(pos.xy * 1113.1 * time);

   color = pixel_color_probes(initial_pos, dir);
   color = pow(color / (color + vec3(1.0)), vec3(0.4545));
}
