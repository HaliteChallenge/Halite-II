#version 400

uniform float width;
uniform float height;

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in vec3 color[];
out vec3 fragColor;

void main()
{
	fragColor = color[0];
	vec4 position = gl_in[0].gl_Position;

	//Add top left vertex.
	gl_Position = position + vec4(-width, -height, 0.0, 0.0);
	EmitVertex();

	//Add bottom left vertex.
	gl_Position = position + vec4(-width, height, 0.0, 0.0);
	EmitVertex();

	//Add top right vertex.
	gl_Position = position + vec4(width, -height, 0.0, 0.0);
	EmitVertex();

	//Add bottom right vertex.
	gl_Position = position + vec4(width, height, 0.0, 0.0);
	EmitVertex();

	//Finsih primitive.
	EndPrimitive();
}