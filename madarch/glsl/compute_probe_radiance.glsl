#include "maths.glsl"
#include "probe_utils.glsl"
#include "random.glsl"
#include "scene.glsl"
#include "materials.glsl"
#include "raymarching.glsl"
#include "cook_torrance_brdf.glsl"
#include "lighting.glsl"
#include "volumetrics.glsl"
#include "render_probes.glsl"

in vec4 pos;

out vec3 color;

void main() {
   vec2 normalized_coord = (pos.xy + vec2(1)) * 0.5;

   int probe_id = coord_to_probe_id(normalized_coord);
   ivec3 grid_position = probe_id_to_grid_position(probe_id);
   vec3 world_position = grid_position_to_world_position(grid_position);

   vec2 ray_id = coord_to_ray_id(normalized_coord);
   vec3 ray_dir = ray_id_to_ray_dir(ray_id);

   color = pixel_color_probes(world_position, ray_dir, normalized_coord);
}
