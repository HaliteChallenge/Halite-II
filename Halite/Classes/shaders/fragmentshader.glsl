#version 400

in vec3 fragColor;
out vec4 frag_colour;

void main ()
{
	frag_colour = vec4(fragColor, 1.0);
	//frag_colour = vec4(1.0, 0.0, 0.0, 1.0);
};