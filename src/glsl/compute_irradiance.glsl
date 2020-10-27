#version 120

#include "raymarch.glsl"

varying vec4 pos;

vec3 sphere_direction(float l1, float l2) {
   float ang1 = l1 * PI_2; // [0..1) -> [0..2*PI)
   float u = l2 * 2 - 1; // [0..1) -> [-1..1)
   float u2 = u * u;
   float sqrt1MinusU2 = sqrt(1.0 - u2);
   float x = sqrt1MinusU2 * cos(ang1);
   float y = sqrt1MinusU2 * sin(ang1);
   float z = u;
   return vec3(x, y, z);
}

vec2 coord_to_ray_id(vec2 normalized_coord) {
   vec2 scaled = vec2(
      normalized_coord.x * probe_count.x,
      normalized_coord.y * probe_count.y
   );
   return scaled - ivec2(scaled);
}

vec3 ray_id_to_ray_dir(vec2 ray_id) {
   return sphere_direction(ray_id.x, ray_id.y);
}

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
