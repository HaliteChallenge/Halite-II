#version 400

in vec2 texcoord;
uniform sampler2D tex;
uniform vec4 color;
out vec4 frag_color;

void main ()
{
	frag_color = vec4(1.0, 1.0, 1.0, texture2D(tex, texcoord).r) * color;
};