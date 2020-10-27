#version 120

#include "probe_utils.glsl"

varying vec4 pos;

uniform sampler2D radiance_data;

void main(void) {
   vec2 normalized_coord = (pos.xy + vec2(1)) * 0.5;
   gl_FragColor = texture2D(radiance_data, normalized_coord);
}
