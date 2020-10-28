#version 120

#include "probe_utils.glsl"

varying vec4 pos;

uniform sampler2D radiance_data;

void main(void) {
   vec2 normalized_coord = (pos.xy + vec2(1)) * 0.5;
   vec2 irr_ray_id = coord_to_ray_id(normalized_coord);
   vec3 irr_ray_dir = ray_id_to_ray_dir(irr_ray_id);

   int probe_id = coord_to_probe_id(normalized_coord);
   vec2 rad_coord = probe_id_to_coord(probe_id);

   vec3 irradiance = vec3(0.0);
   float total_weight = 0.0;

   const vec2 step = vec2(1) / vec2(
      probe_count.x * radiance_resolution,
      probe_count.y * radiance_resolution
   );

   for (int y = 0; y < radiance_resolution; ++y) {
      for (int x = 0; x < radiance_resolution; ++x) {
         vec2 offset = vec2(x, y) * step;
         vec2 rad_normalized_coord = rad_coord + offset;
         vec3 radiance = texture2D(radiance_data, rad_normalized_coord).rgb;
         vec2 rad_ray_id = coord_to_ray_id(rad_normalized_coord);
         vec3 rad_ray_dir = ray_id_to_ray_dir(rad_ray_id);

         float weight = max(dot(irr_ray_dir, rad_ray_dir), 0);
         irradiance += radiance * weight;
         total_weight += weight;
      }
   }

   irradiance /= total_weight;

   gl_FragColor = vec4(irradiance, 1);
}
