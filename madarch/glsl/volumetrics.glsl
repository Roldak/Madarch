const int visibility_resolution_z = M_VISIBILITY_RESOLUTION_Z;
const float visibility_step_size = M_VISIBILITY_STEP_SIZE;
const float visibility_max_depth =
   visibility_step_size * visibility_resolution_z;

const float scattering_step_size = M_SCATTERING_STEP_SIZE;
const vec2 scattering_data_step = vec2(1) / vec2(
   M_SCATTERING_RESOLUTION_X,
   M_SCATTERING_RESOLUTION_Y
);

const float tau_scattering = 0.1;

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

vec3 render_volumetrics(vec3 L, vec3 from, vec3 to, vec2 frag_pos) {
#if M_RENDER_VOLUMETRICS
   vec2 tex_coord = (frag_pos + vec2(1)) * 0.5;
   float len = length(to - from);
   float closest = max_dist;
   vec3 fog_L;
   for (int x = -1; x <= 1; ++x) {
      for (int y = -1; y <= 1; ++y) {
         vec2 offset = vec2(x, y) * scattering_data_step;
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

