struct partition_info {
    int Sphere_count;
    int Sphere_indices[10];

    int Plane_count;
    int Plane_indices[10];
};

layout(std140, binding=0) buffer partition_data_buffer {
   partition_info partition_data[];
};

float partitioning_closest (vec3 x) {
   vec3 fx = floor(x);
   int data_index = int(fx.x * 100 + fx.y * 10 + fx.z);
   if (fx != clamp(fx, vec3(0), vec3(9))) {
      return closest_primitive(x);
   }

   float closest = max_dist;

   for (int i = 0; i < partition_data[data_index].Sphere_count; ++i) {
      closest = min(closest, dist_to_Sphere (prim_Spheres[partition_data[data_index].Sphere_indices[i]], x));
   }
   for (int i = 0; i < partition_data[data_index].Plane_count; ++i) {
      closest = min(closest, dist_to_Plane (prim_Planes[partition_data[data_index].Plane_indices[i]], x));
   }
   return closest;
}

float partitioning_closest_info(vec3 x, out int index) {
   vec3 fx = floor(x);
   int data_index = int(fx.x * 100 + fx.y * 10 + fx.z);
   if (fx != clamp(fx, vec3(0), vec3(9))) {
      return closest_primitive_info(x, index);
   }

   float closest = max_dist;

   for (int i = 0; i < partition_data[data_index].Sphere_count; ++i) {
      float dist = dist_to_Sphere (prim_Spheres[partition_data[data_index].Sphere_indices[i]], x);
      if (dist < closest) {
         closest = dist;
         index = partition_data[data_index].Sphere_indices[i];
      }
   }
   for (int i = 0; i < partition_data[data_index].Plane_count; ++i) {
      float dist = dist_to_Plane (prim_Planes[partition_data[data_index].Plane_indices[i]], x);
      if (dist < closest) {
         closest = dist;
         index = 20 + partition_data[data_index].Plane_indices[i];
      }
   }
   return closest;
}
