struct partition_info {
    int Sphere_count;
    int Sphere_indices[20];

    int Plane_count;
    int Plane_indices[20];
};

layout(std140, binding=0) buffer partition_data_buffer {
   partition_info partition_data[];
};

float partitioning_closest (vec3 x) {
   vec3 fx = clamp(floor(x) + vec3(1, 1, 1), vec3(0), vec3(9));
   int data_index = int(fx.x * 100 + fx.y * 10 + fx.z);

   float closest = max_dist;

   int Sphere_count = partition_data[data_index].Sphere_count;
   for (int i = 0; i < Sphere_count; ++i) {
      closest = min(closest, dist_to_Sphere (prim_Spheres[partition_data[data_index].Sphere_indices[i]], x));
   }

   int Plane_count = partition_data[data_index].Plane_count;
   for (int i = 0; i < Plane_count; ++i) {
      closest = min(closest, dist_to_Plane (prim_Planes[partition_data[data_index].Plane_indices[i]], x));
   }
   return closest;
}

float partitioning_closest_info(vec3 x, out int index) {
   vec3 fx = clamp(floor(x) + vec3(1, 1, 1), vec3(0), vec3(9));
   int data_index = int(fx.x * 100 + fx.y * 10 + fx.z);

   float closest = max_dist;

   int Sphere_count = partition_data[data_index].Sphere_count;
   for (int i = 0; i < Sphere_count; ++i) {
      int Sphere_index = partition_data[data_index].Sphere_indices[i];
      float dist = dist_to_Sphere (prim_Spheres[Sphere_index], x);
      if (dist < closest) {
         closest = dist;
         index = Sphere_index;
      }
   }

   int Plane_count = partition_data[data_index].Plane_count;
   for (int i = 0; i < Plane_count; ++i) {
      int Plane_index = partition_data[data_index].Plane_indices[i];
      float dist = dist_to_Plane (prim_Planes[Plane_index], x);
      if (dist < closest) {
         closest = dist;
         index = M_MAX_SPHERE_COUNT + Plane_index;
      }
   }
   return closest;
}
