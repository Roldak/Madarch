vec3 pixel_color_direct(vec3 from, vec3 dir) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   if (raycast(from, dir, prim_index, pos)) {
      int material_id;
      vec3 normal;
      primitive_info (prim_index, pos, normal, material_id);
      vec3 albedo = materials[material_id].albedo;
      float metallic = materials[material_id].metallic;
      float roughness = materials[material_id].roughness;
      return compute_direct_lighting(pos, normal, dir, albedo, metallic, roughness);
   }

   return background_color;
}

