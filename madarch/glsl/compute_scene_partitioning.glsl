#include "maths.glsl"
#include "scene.glsl"
#include "raymarching.glsl"

layout(local_size_x = 2, local_size_y = 2, local_size_z = 2) in;

void main() {
#if PARTITIONING_ENABLED
   uvec3 grid_size = gl_WorkGroupSize * gl_NumWorkGroups;
   uvec3 cell_id = gl_GlobalInvocationID;
   int data_index = int(
      cell_id.x * grid_size.y * grid_size.z +
      cell_id.y * grid_size.z +
      cell_id.z
   );

   partition_info info;
   partitioning_compute_grid_cell (cell_id, info);
   partition_data[data_index] = info;
#endif
}

