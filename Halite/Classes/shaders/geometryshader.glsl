#version 400

uniform float width;
uniform float height;

layout(points) in;
layout(triangle_strip, max_vertices = 12) out;

in vec3 color[];
in uint strength[];
out vec3 fragColor;

void main()
{
	uint s = strength[0];
	vec4 position = gl_in[0].gl_Position;
	vec3 c = color[0];

	//Find top left vertex.
	vec4 tlPosition = position + vec4(-width, height, 0.0, 0.0);

	//Find bottom left vertex.
	vec4 blPosition = position + vec4(-width, -height, 0.0, 0.0);

	//Find top right vertex.
	vec4 trPosition = position + vec4(width, height, 0.0, 0.0);

	//Find bottom right vertex.
	vec4 brPosition = position + vec4(width, -height, 0.0, 0.0);

	//Generate bottom triangle
	fragColor = c * (.25 + (uint(s / 64) / 4.0));
	gl_Position = brPosition;
	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = blPosition;
	EmitVertex();
	
	//Range is now 0 to 64:
	s %= 64;
	
	//Generate left triangle
	fragColor = c * (.25 + (uint(s / 16) / 4.0));
	//Position can remain the same.
	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = tlPosition;
	EmitVertex();

	//Range is now 0 to 16:
	s %= 16;
	
	//Generate top triangle
	fragColor = c * (.25 + (uint(s / 4) / 4.0));
	//Position can remain the same.
	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = trPosition;
	EmitVertex();

	//Range is now 0 to 4:
	s %= 4;
	
	//Generate right triangle
	fragColor = c * (.25 + (s / 4.0));
	//Position can remain the same.
	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = brPosition;
	EmitVertex();
	EndPrimitive();
}