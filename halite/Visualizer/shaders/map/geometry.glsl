#version 400

uniform float width;
uniform float height;

layout(points) in;
layout(triangle_strip, max_vertices = 16) out;

in vec3 color[];
in uint strength[];
out vec3 fragColor;

void main()
{
	uint s = strength[0];
	vec4 position = gl_in[0].gl_Position;
	vec3 c = 0.2 * color[0];
	
	//Declare float which represents dialation factor of inner square:
	float dialationFactor = 0.85;

	//Find top left vertices.
	vec4 tlBPosition = position + vec4(-width, height, 0.0, 0.0);

	//Find bottom left vertices.
	vec4 blBPosition = position + vec4(-width, -height, 0.0, 0.0);

	//Find top right vertices.
	vec4 trBPosition = position + vec4(width, height, 0.0, 0.0);

	//Find bottom right vertices.
	vec4 brBPosition = position + vec4(width, -height, 0.0, 0.0);

	//NEW METHOD:

	//Create outer square:
	fragColor = c;
	gl_Position = blBPosition;
	EmitVertex();
	gl_Position = brBPosition;
	EmitVertex();
	gl_Position = tlBPosition;
	EmitVertex();
	gl_Position = trBPosition;
	EmitVertex();

	float newdialationFactor = sqrt(float(s) / 255.0);

	//Find top left vertices.
	vec4 tlLPosition = position + vec4(-width*newdialationFactor, height*newdialationFactor, 0.0, 0.0);

	//Find bottom left vertices.
	vec4 blLPosition = position + vec4(-width*newdialationFactor, -height*newdialationFactor, 0.0, 0.0);

	//Find top right vertices.
	vec4 trLPosition = position + vec4(width*newdialationFactor, height*newdialationFactor, 0.0, 0.0);

	//Find bottom right vertices.
	vec4 brLPosition = position + vec4(width*newdialationFactor, -height*newdialationFactor, 0.0, 0.0);

	//Degenerate triangles:
	EmitVertex();
	gl_Position = brLPosition;
	EmitVertex();

	fragColor = color[0];
	EmitVertex();
	gl_Position = blLPosition;
	EmitVertex();
	gl_Position = trLPosition;
	EmitVertex();
	gl_Position = tlLPosition;
	EmitVertex();

	EndPrimitive();
}