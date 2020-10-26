#version 120

varying vec4 pos;

uniform float time;
uniform sampler2D irradiance_data;

const float PI_2 = 6.28318530718;

const int probe_resolution = 10;
const ivec2 probe_count = ivec2(4, 4);
const ivec3 grid_dimensions = ivec3(4, 2, 2);
const vec3 grid_spacing = vec3(1.0, 1.0, 1.0);

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

int coord_to_probe_id(vec2 normalized_coord) {
   ivec2 probe_id = ivec2(
      int(normalized_coord.x * probe_count.x),
      int(normalized_coord.y * probe_count.y)
   );
   return probe_id.y * probe_count.x + probe_id.x;
}

ivec3 probe_id_to_grid_position(int probe_id) {
   int xy_count = grid_dimensions.x * grid_dimensions.y;
   int x_count = grid_dimensions.x;

   int z_offset = probe_id / xy_count;
   int y_offset = (probe_id - z_offset * xy_count) / x_count;
   int x_offset = probe_id - z_offset * xy_count - y_offset * x_count;

   return ivec3(x_offset, y_offset, z_offset);
}

vec3 grid_position_to_world_position(ivec3 grid_position) {
   return grid_position * grid_spacing;
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

   vec4 last = texture2D(irradiance_data, normalized_coord);
   vec4 curr = vec4(ray_dir, 1);

   gl_FragColor = curr - last;
}
