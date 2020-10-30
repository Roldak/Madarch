const ivec2 probe_count = ivec2(6, 6);
const ivec3 grid_dimensions = ivec3(4, 3, 3);
const vec3 grid_spacing = vec3(2.0, 3.0, 3.0);

const int radiance_resolution = 30;
const int irradiance_resolution = 8;

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

ivec3 world_position_to_grid_position(vec3 world_pos) {
   return ivec3(floor(world_pos / grid_spacing));
}

int grid_position_to_probe_id(ivec3 grid_position) {
   return grid_position.z * grid_dimensions.x * grid_dimensions.y
       +  grid_position.y * grid_dimensions.x
       +  grid_position.x;
}

vec2 probe_id_to_coord(int probe_id) {
   int y_coord = probe_id / probe_count.x;
   int x_coord = probe_id - y_coord * probe_count.x;
   return vec2(x_coord, y_coord) / probe_count;
}

// Returns ±1
vec2 sign_not_zero(vec2 v) {
   return vec2((v.x >= 0.0) ? +1.0 : -1.0, (v.y >= 0.0) ? +1.0 : -1.0);
}

//Assume normalized input.  Output is on [-1, 1] for each component.
vec2 float32x3_to_oct(in vec3 v) {
   //Project the sphere onto the octahedron, and then onto the xy plane
   vec2 p = v.xy * (1.0 / (abs(v.x) + abs(v.y) + abs(v.z)));

   //Reflect the folds of the lower hemisphere over the diagonals
   return (v.z <= 0.0) ? ((1.0 - abs(p.yx)) * sign_not_zero(p)) : p;
}

vec3 oct_to_float32x3(vec2 e) {
   vec3 v = vec3(e.xy, 1.0 - abs(e.x) - abs(e.y));
   if (v.z < 0) {
       v.xy = (1.0 - abs(v.yx)) * sign_not_zero(v.xy);
   }
   return normalize(v);
}

vec2 coord_to_ray_id(vec2 normalized_coord) {
   return fract(normalized_coord * probe_count);
}

vec3 ray_id_to_ray_dir(vec2 ray_id) {
   vec2 norm = ray_id * 2 - vec2(1);
   return oct_to_float32x3(norm);
}

vec2 ray_dir_to_ray_id(vec3 ray_dir) {
   vec2 raw = float32x3_to_oct(ray_dir);
   return (raw + vec2(1)) * 0.5;
}

