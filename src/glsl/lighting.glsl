vec3 compute_direct_lighting(vec3 pos, vec3 normal, vec3 dir,
                             vec3 albedo, float metallic, float roughness) {
   vec3 N = normal;
   vec3 V = -dir;

   vec3 Lo = vec3(0.0);

   // do for each light:
   vec3 light_dir = point_light.position - pos;
   float light_distance = length(light_dir);
   vec3 L = light_dir / light_distance;

   float NdotL = max(dot(N, L), 0.0);

   // calculate light radiance
   float attenuation = 1.0 / (light_distance * light_distance * 0.03);
   vec3 radiance     = point_light.color * min(attenuation, 1.5);

   // sample cook-torrance BRDF
   vec3 kD, kS;
   cook_torrance_coefficients (N, V, L, NdotL, albedo, metallic, roughness, kD, kS);

   // shadow cast
   float shadows = 0.0;
   if (NdotL > epsilon) {
      shadows = softshadows(
         pos + normal * min_step_size * 5,
         L,
         0.0,
         light_distance,
         64
      );
   }

#if M_COMPUTE_DIRECT_SPECULAR == 0
   kS = vec3(0);
#endif

   // add to outgoing radiance Lo
   Lo += (kD * albedo / PI + kS) * radiance * NdotL * shadows;

   return Lo;
}

vec3 compute_indirect_lighting(vec3 irradiance, vec3 radiance,
                               vec3 V, vec3 N, vec3 L,
                               vec3 albedo, float metallic, float roughness) {
   vec3 kD, kS;
   float NdotL = max(dot(N, L), 0.0);
   cook_torrance_coefficients (N, V, L, NdotL, albedo, metallic, roughness, kD, kS);
   return (kD * irradiance / PI) + (kS * radiance * NdotL);
}

