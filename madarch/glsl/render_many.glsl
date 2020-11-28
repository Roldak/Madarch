#include "render_direct.glsl"

const int gi_samples = 100;

vec3 pixel_color_many(vec3 from, vec3 dir, float sa) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;

   vec3 result;
   if (!raycast(from, dir, prim_index, pos)) {
      return background_color;
   }

   int material_id;
   vec3 normal;
   primitive_info (prim_index, pos, normal, material_id);
   vec3 albedo = materials[material_id].albedo;
   float metallic = materials[material_id].metallic;
   float roughness = materials[material_id].roughness;
   result = compute_direct_lighting(pos, normal, dir, albedo, metallic, roughness);

   from = pos + normal * min_step_size * 5;
   vec3 reflected = reflect(dir, normal);

   vec3 acc = vec3(0.0);

   for (int s = 0; s < gi_samples; ++s) {
      // BRDF
      if (rand(vec2(sa + time * 127.2, sa + 7.7 * float(s))) < roughness) {
         dir = random_hemisphere_point(sa + 76.2 + 73.1 * float(s) + 17.7 * time, normal);
      } else {
         vec3 offset = uniform_vector(sa + time * 111.123 + 65.2 * float(s));
         dir = normalize(reflected + offset * roughness);
      }
      acc += pixel_color_direct(from, dir) * abs(dot(dir, normal));
   }

   return result + acc / gi_samples;
}

