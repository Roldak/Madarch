#version 140

#include "raymarch.glsl"

in vec4 pos;

out vec3 color;

void main() {
   vec2 normalized_coord = (pos.xy + vec2(1)) * 0.5;

   int probe_id = coord_to_probe_id(normalized_coord);
   ivec3 grid_position = probe_id_to_grid_position(probe_id);
   vec3 world_position = grid_position_to_world_position(grid_position);

   vec2 ray_id = coord_to_ray_id(normalized_coord);
   vec3 ray_dir = ray_id_to_ray_dir(ray_id);

   vec3 light_pos = vec3(3.0 + cos(time), 2, sin(time));
   color = pixel_color_irradiance_probes(world_position, ray_dir, light_pos);
}
