#version 400

uniform float width;
uniform float height;

layout(points) in;
layout(triangle_strip, max_vertices = 10) out;

in vec3 color[];
in uint strength[];
out vec3 fragColor;

void main()
{
	uint s = strength[0];
	vec4 position = gl_in[0].gl_Position;
	vec3 c = color[0];
	
	//Declare float which represents dialation factor of inner square:
	float dialationFactor = 1.2;

	//Find top left vertices.
	vec4 tlBPosition = position + vec4(-width, height, 0.0001, 0.0), tlLPosition = position + vec4(-width/dialationFactor, height/dialationFactor, 0.0, 0.0);

	//Find bottom left vertices.
	vec4 blBPosition = position + vec4(-width, -height, 0.0001, 0.0), blLPosition = position + vec4(-width/dialationFactor, -height/dialationFactor, 0.0, 0.0);

	//Find top right vertices.
	vec4 trBPosition = position + vec4(width, height, 0.0001, 0.0), trLPosition = position + vec4(width/dialationFactor, height/dialationFactor, 0.0, 0.0);

	//Find bottom right vertices.
	vec4 brBPosition = position + vec4(width, -height, 0.0001, 0.0), brLPosition = position + vec4(width/dialationFactor, -height/dialationFactor, 0.0, 0.0);

	//Set color:
	fragColor = c;

	gl_Position = brBPosition;
	EmitVertex();
	gl_Position = blBPosition;
	EmitVertex();
	gl_Position = trBPosition;
	EmitVertex();
	gl_Position = tlBPosition;
	EmitVertex();

	//Repeat vertex on each side to generate degenerate triangles:
	EmitVertex();

	gl_Position = tlLPosition;
	EmitVertex();
	
	//Find color:
	float val = .1 + (.9 * sqrt(float(s)/255.0));
	fragColor = c * val;

	EmitVertex();
	gl_Position = trLPosition;
	EmitVertex();
	gl_Position = blLPosition;
	EmitVertex();
	gl_Position = brLPosition;
	EmitVertex();

	EndPrimitive();
}