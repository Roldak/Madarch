const int gi_bounces = 3;

vec3 pixel_color_path(vec3 from, vec3 dir, float sa) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);
   vec3 result = vec3(0.0);
   vec3 mask = vec3(1.0);

   for (int bounce = 0; bounce < gi_bounces + 1; ++bounce) {
      int prim_index;
      vec3 pos;
      if (raycast(from, dir, prim_index, pos)) {
         int material_id;
         vec3 normal;
         primitive_info (prim_index, pos, normal, material_id);
         vec3 albedo = materials[material_id].albedo;
         float metallic = materials[material_id].metallic;
         float roughness = materials[material_id].roughness;

         result += mask * compute_direct_lighting (
            pos, normal, dir, albedo, metallic, roughness
         );
         mask *= albedo;

         from = pos;

         // BRDF
         if (rand(vec2(sa + time * 127.2, sa + 7.7 * float(bounce))) < roughness) {
            dir = cosine_direction(sa + 76.2 + 73.1 * float(bounce) + 17.7 * time, normal);
         } else {
            vec3 reflected = reflect(dir, normal);
            vec3 offset = uniform_vector(sa + time * 111.123 + 65.2 * float(bounce));
            dir = normalize(reflected + offset * roughness);
         }
      } else {
         result += background_color;
      }
   }
   return result;
}

