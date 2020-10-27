#version 120

#include "raymarch.glsl"

varying vec4 pos;

void main(void) {
   vec2 normalized_coord = (pos.xy + vec2(1)) * 0.5;

   int probe_id = coord_to_probe_id(normalized_coord);
   ivec3 grid_position = probe_id_to_grid_position(probe_id);
   vec3 world_position = grid_position_to_world_position(grid_position);

   vec2 ray_id = coord_to_ray_id(normalized_coord);
   vec3 ray_dir = ray_id_to_ray_dir(ray_id);

   vec3 light_pos = vec3(cos(time), 2, sin(time));
   vec3 col = pixel_color_direct(world_position, ray_dir, light_pos);

   gl_FragColor = vec4(col, 1);
}
