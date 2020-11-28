vec3 compute_direct_lighting(vec3 pos, vec3 normal, vec3 dir,
                             vec3 albedo, float metallic, float roughness) {
   vec3 N = normal;
   vec3 V = -dir;

   vec3 Lo = vec3(0.0);

   for (int i = 0; i < light_count; ++i) {
      vec3 L;
      float L_dist;
      vec3 radiance = sample_light(i, pos, N, L, L_dist);

      float NdotL = max(dot(N, L), 0.0);

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
            L_dist,
            64
         );
      }

#if M_COMPUTE_DIRECT_SPECULAR == 0
      kS = vec3(0);
#endif

      // add to outgoing radiance Lo
      Lo += (kD * albedo / PI + kS) * radiance * NdotL * shadows;
   }

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

float compute_ambient_occlusion(vec3 pos, vec3 normal) {
#if M_AMBIENT_OCCLUSION_STEPS > 0
   const float ao_step_size = 0.1;

   float ao_sum = 0.0;
   float max_ao_sum = 0.0;

   for (int i = 0; i < M_AMBIENT_OCCLUSION_STEPS; ++i) {
      vec3 p = pos + normal * (i + 1) * ao_step_size;
      float factor = 1.0 / pow(2.0, i);
      ao_sum += factor * closest_primitive(p);
      max_ao_sum += factor * (i + 1) * ao_step_size;
   }

   return 0.6 + 0.4 * ao_sum / max_ao_sum;
#else
   return 1.0;
#endif
}
