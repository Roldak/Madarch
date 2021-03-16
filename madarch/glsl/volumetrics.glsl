const float tau_scattering = 0.1;
const float light_shafts_step_length = 0.1f;

float rayleigh_phase(vec3 in_dir, vec3 out_dir) {
   float cos_angle = dot(in_dir, out_dir);
   float nom = 3.0f * (1.0f + cos_angle * cos_angle);
   float denom = 16.0f * PI;
   return nom / denom;
}

float henvey_greenstein_phase(vec3 in_dir, vec3 out_dir) {
   float cos_angle = dot(in_dir, out_dir);
   float tau_scattering_squared = tau_scattering * tau_scattering;
   float result = 1.0 - tau_scattering_squared;
   result /= 4.0 * PI * pow(
      1.0 + tau_scattering_squared - 2 * tau_scattering * cos_angle,
      1.5
   );
   return result;
}

layout(binding = 3) uniform sampler2D scattering_data;

vec3 compute_light_shafts(vec3 L, vec3 from, vec3 to, vec2 frag_pos) {
#if M_RENDER_LIGHT_SHAFTS
   vec2 tex_coord = (frag_pos + vec2(1)) * 0.5;
   const vec2 step = vec2(1) / vec2(250, 250);
   float len = length(to - from);
   float closest = max_dist;
   vec3 fog_L;
   for (int x = -1; x <= 1; ++x) {
      for (int y = -1; y <= 1; ++y) {
         vec2 offset = vec2(x, y) * step;
         vec4 data = texture2D(scattering_data, tex_coord + offset);
         float dist = abs(data.a - len);
         if (dist < closest) {
            closest = dist;
            fog_L = data.rgb;
         }
      }
   }
   L = L * exp(-len * tau_scattering) + fog_L;
#endif
   return L;
}

