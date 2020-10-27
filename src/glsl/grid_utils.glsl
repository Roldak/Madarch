const int probe_resolution = 10;
const ivec2 probe_count = ivec2(4, 4);
const ivec3 grid_dimensions = ivec3(4, 2, 2);
const vec3 grid_spacing = vec3(1.0, 1.0, 1.0);

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
   return ivec3(world_pos / grid_spacing);
}

int grid_position_to_probe_id(ivec3 grid_position) {
   return grid_position.z * grid_dimensions.x * grid_dimensions.y
       +  grid_position.y * grid_dimensions.x
       +  grid_position.x;
}

