const float min_step_size = 0.005f;
const int max_steps = 300;

float softshadows(vec3 from, vec3 dir, float min_dist, float max_dist, float k) {
   float res = 1.0;
   float prev_dist = 1e20;

   for (float total_dist = min_dist; total_dist < max_dist;) {
      float dist = partitioning_closest(from + dir * total_dist);

      if (dist < epsilon) {
         return 0.0;
      }

      float y = dist * dist / (2.0 * prev_dist);
      float d = sqrt(dist * dist - y * y);
      res = min(res, k * d / max(0.0, total_dist - y));

      prev_dist = dist;
      total_dist += dist;
   }
   return res;
}

bool raycast(vec3 from, vec3 dir, out int index, out vec3 coll) {
   for (float total_dist = 0; total_dist < max_dist;) {
      float dist = partitioning_closest_info(from + dir * total_dist, index);

      if (dist < epsilon) {
         coll = from + dir * total_dist;
         return true;
      }

      total_dist += dist;
   }
   return false;
}

bool raycast_hit_position(vec3 from, vec3 dir, float max_dist, out vec3 coll) {
   for (float total_dist = 0; total_dist < max_dist;) {
      float dist = partitioning_closest(from + dir * total_dist);

      if (dist < epsilon) {
         coll = from + dir * total_dist;
         return true;
      }

      total_dist += dist;
   }
   return false;
}

float raycast_visibility(vec3 from, vec3 dir, float max_dist) {
   vec3 dummy;
   return 1.0 - float(raycast_hit_position(from, dir, max_dist, dummy));
}

