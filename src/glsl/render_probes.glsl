#extension GL_EXT_gpu_shader4 : enable

layout(binding = 0) uniform sampler2D radiance_data;
layout(binding = 1) uniform sampler2D irradiance_data;

vec3 sample_irradiance(vec3 pos, vec3 normal) {
   ivec3 grid_position = world_position_to_grid_position(pos);
   vec3 irradiance = vec3(0);
   float total_weight = 0.0;

   vec3 alpha = pos / grid_spacing - grid_position;

   for (int i = 0; i < 8; ++i) {
      ivec3 offset = ivec3(i, i >> 1, i >> 2) & ivec3(1);

      ivec3 offseted = clamp(
         grid_position + offset,
         ivec3(0),
         ivec3(grid_dimensions) - ivec3(1)
      );

      vec3 hit_to_probe = grid_position_to_world_position(offseted) - pos;
      float probe_distance = length(hit_to_probe);
      vec3 dir_to_probe = hit_to_probe / probe_distance;

      float weight = 1.0;

      // backface test
      float angle = (dot(dir_to_probe, normal) + 1.0) * 0.5;
      weight *= angle * angle + 0.2;

      // visibility test
      weight *= raycast_visibility(
         pos + normal * min_step_size * 5.0,
         dir_to_probe,
         probe_distance - min_step_size * 5.0
      );

      // crush tiny weights
      const float crushThreshold = 0.2;
      if (weight < crushThreshold) {
          weight *= weight * weight * (1.0 / (crushThreshold * crushThreshold));
      }

      // trilinear weights
      vec3 trilinear = mix(1.0 - alpha, alpha, offset);
      weight *= trilinear.x * trilinear.y * trilinear.z;

      // retrieve irradiance
      int probe_id = grid_position_to_probe_id(offseted);
      vec2 irr_base_coord = probe_id_to_coord(probe_id);

      vec2 irr_ray_dir_id = clamp(
         ray_dir_to_ray_id(normal),
         irr_min_coord,
         irr_max_coord
      );

      vec2 irr_coord = irr_base_coord + irr_ray_dir_id / probe_count;

      irradiance += sqrt(texture2D(irradiance_data, irr_coord).rgb) * weight;
      total_weight += weight;
   }

   irradiance /= total_weight;
   irradiance *= irradiance;

   return irradiance;
}

vec3 sample_radiance_with_specular(vec3 pos, vec3 normal, vec3 dir, float roughness) {
   vec3 spec_pos;
   vec3 from = pos + normal * min_step_size * 5.0;
   if (!raycast_hit_position(from, dir, max_dist, spec_pos)) {
      return vec3(0);
   }

   vec3 pos_to_spec_pos = spec_pos - pos;

   ivec3 grid_position = world_position_to_grid_position(pos);
   vec3 alpha = pos / grid_spacing - grid_position;

   // this shouldn't be dependent on the radiance resolution
   float lod = mix(0.0, float(radiance_lods), roughness * 2.0);
   int new_res = radiance_resolution / int(lod + 1.0);
   vec2 rad_min_coord = vec2(0.5) / new_res;
   vec2 rad_max_coord = vec2(1.0) - rad_min_coord;

   float total_weight = 0.0f;
   vec3 radiance = vec3(0);

   for (int i = 0; i < 8; ++i) {
      ivec3 offset = ivec3(i, i >> 1, i >> 2) & ivec3(1);

      ivec3 offseted = clamp(
         grid_position + offset,
         ivec3(0),
         ivec3(grid_dimensions) - ivec3(1)
      );

      vec3 probe_pos = grid_position_to_world_position(offseted);
      vec3 probe_to_pos = pos - probe_pos;
      vec3 probe_to_spec = probe_to_pos + pos_to_spec_pos;
      float distance = length(probe_to_spec);
      probe_to_spec /= distance;

      // visibility test
      float weight = max(softshadows(
         spec_pos,
         -probe_to_spec,
         min_step_size * 5.0,
         distance - min_step_size * 5.0,
         0.5
      ), 0.001);

      vec3 trilinear = mix(1.0 - alpha, alpha, offset);
      weight *= trilinear.x * trilinear.y * trilinear.z;

      // retrieve radiance
      int probe_id = grid_position_to_probe_id(offseted);
      vec2 rad_base_coord = probe_id_to_coord(probe_id);

      vec2 rad_ray_dir_id = clamp(
         ray_dir_to_ray_id(probe_to_spec),
         rad_min_coord,
         rad_max_coord
      );
      vec2 rad_coord = rad_base_coord + rad_ray_dir_id / probe_count;

      radiance += textureLod(radiance_data, rad_coord, lod).rgb * weight;
      total_weight += weight;
   }

   radiance /= total_weight;
   return radiance;
}

vec3 sample_radiance_no_specular(vec3 pos, vec3 normal, vec3 dir, float roughness) {
   int prim_index;
   vec3 spec_pos;
   vec3 from = pos + normal * min_step_size * 5.0;
   if (!raycast(from, dir, prim_index, spec_pos)) {
      return vec3(0);
   }

   vec3 spec_normal;
   int spec_material_id;
   primitive_info (spec_pos, prim_index, spec_normal, spec_material_id);

   ivec3 grid_position = world_position_to_grid_position(spec_pos);

   float max_weight = -2.0f;
   ivec3 best_offseted;
   vec3 best_probe_to_spec;

   for (int i = 0; i < 8; ++i) {
      ivec3 offset = ivec3(i, i >> 1, i >> 2) & ivec3(1);

      ivec3 offseted = clamp(
         grid_position + offset,
         ivec3(0),
         ivec3(grid_dimensions) - ivec3(1)
      );

      vec3 probe_pos = grid_position_to_world_position(offseted);
      vec3 probe_to_spec = spec_pos - probe_pos;
      float distance = length(probe_to_spec);
      probe_to_spec /= distance;

      float weight = dot(probe_to_spec, -spec_normal);

      // visibility test
      weight *= raycast_visibility(
         spec_pos + spec_normal * min_step_size * 5.0,
         -probe_to_spec,
         distance - min_step_size * 5.0
      );

      //vec3 trilinear = mix(1.0 - alpha, alpha, offset);
      //weight *= trilinear.x * trilinear.y * trilinear.z;

      if (weight > max_weight) {
         max_weight = weight;
         best_offseted = offseted;
         best_probe_to_spec = probe_to_spec;
      }
   }

   // retrieve radiance
   int probe_id = grid_position_to_probe_id(best_offseted);
   vec2 rad_base_coord = probe_id_to_coord(probe_id);

   vec2 rad_ray_dir_id = clamp(
      ray_dir_to_ray_id(best_probe_to_spec),
      rad_min_coord,
      rad_max_coord
   );
   vec2 rad_coord = rad_base_coord + rad_ray_dir_id / probe_count;

   return textureLod(radiance_data, rad_coord, 1.0).rgb;
}

vec3 pixel_color_probes(vec3 from, vec3 dir) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   if (raycast(from, dir, prim_index, pos)) {
      int material_id;
      vec3 normal;
      primitive_info (pos, prim_index, normal, material_id);
      vec3 albedo = materials[material_id].albedo;
      float metallic = materials[material_id].metallic;
      float roughness = materials[material_id].roughness;
      vec3 direct = compute_direct_lighting(pos, normal, dir, albedo, metallic, roughness);

      vec3 irradiance = sample_irradiance(pos, normal);

      vec3 specular_col = vec3(0);
      vec3 specular_dir = reflect(dir, normal);

#if M_COMPUTE_INDIRECT_SPECULAR > 0
      if (roughness < 0.75) {
#if M_COMPUTE_INDIRECT_SPECULAR == 1
         specular_col = sample_radiance_with_specular(pos, normal, specular_dir, roughness);
#else
         specular_col = sample_radiance_no_specular(pos, normal, specular_dir, roughness);
#endif
      }
#endif

      vec3 indirect = compute_indirect_lighting(
         irradiance, specular_col,
         -dir, normal, specular_dir,
         albedo, metallic, roughness
      );

      return indirect + direct;
   }

   return background_color;
}

