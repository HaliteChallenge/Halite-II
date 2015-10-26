#version 400

in vec3 fragColor;
out vec4 frag_color;

void main ()
{
	frag_color = vec4(fragColor, 1.0);
};