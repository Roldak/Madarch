#version 120

varying vec4 pos;

uniform sampler2D radiance_data;

void main(void) {
   vec3 frag_pos = vec3(pos.xy, 0);
   gl_FragColor = texture2D(radiance_data, (frag_pos.xy + vec2(1)) * 0.5);
}
