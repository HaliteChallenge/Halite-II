#version 400

uniform float width;
uniform float height;

layout(points) in;
layout(triangle_strip, max_vertices = 22) out;

in vec3 color[];
in uint strength[];
out vec3 fragColor;

void main()
{
	uint s = strength[0];
	vec4 position = gl_in[0].gl_Position;
	vec3 c = color[0];
	
	//Declare float which represents dialation factor of inner square:
	float dialationFactor = 1.25;

	//Find top left vertices.
	vec4 tlBPosition = position + vec4(-width, height, 0.0, 0.0), tlLPosition = position + vec4(-width/dialationFactor, height/dialationFactor, 0.0, 0.0);

	//Find bottom left vertices.
	vec4 blBPosition = position + vec4(-width, -height, 0.0, 0.0), blLPosition = position + vec4(-width/dialationFactor, -height/dialationFactor, 0.0, 0.0);

	//Find top right vertices.
	vec4 trBPosition = position + vec4(width, height, 0.0, 0.0), trLPosition = position + vec4(width/dialationFactor, height/dialationFactor, 0.0, 0.0);

	//Find bottom right vertices.
	vec4 brBPosition = position + vec4(width, -height, 0.0, 0.0), brLPosition = position + vec4(width/dialationFactor, -height/dialationFactor, 0.0, 0.0);

	//Find top, left, bottom, and right vertices.
	//vec4 tPosition = position + vec4(0.0, height/dialationFactor, 0.0, 0.0), lPosition = position + vec4(-width/dialationFactor, 0.0, 0.0, 0.0), bPosition = position + vec4(0.0, -height/dialationFactor, 0.0, 0.0), rPosition = position + vec4(width/dialationFactor, 0.0, 0.0, 0.0);

	//NEW METHOD:

	//Create outer square:
	fragColor = c;
	gl_Position = brBPosition;
	EmitVertex();
	gl_Position = brLPosition;
	EmitVertex();
	gl_Position = trBPosition;
	EmitVertex();
	gl_Position = trLPosition;
	EmitVertex();
	gl_Position = tlBPosition;
	EmitVertex();
	gl_Position = tlLPosition;
	EmitVertex();
	gl_Position = blBPosition;
	EmitVertex();
	gl_Position = blLPosition;
	EmitVertex();
	gl_Position = brBPosition;
	EmitVertex();
	gl_Position = brLPosition;
	EmitVertex();

	//Find color of upper left square:
	float color = float(s / 64) / 3.0;
	fragColor = vec3(color, color, color);

	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = blLPosition;
	EmitVertex();

	//Range is now 0 to 64;
	s %= 64;

	//Find color of upper right
	color = float(s / 16) / 3.0;
	fragColor = vec3(color, color, color);

	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = tlLPosition;
	EmitVertex();

	//Range is now 0 to 16;
	s %= 16;

	//Find color of lower left
	color = float(s / 4) / 3.0;
	fragColor = vec3(color, color, color);
	
	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = trLPosition;
	EmitVertex();

	//Range is now 0 to 4;
	s %= 4;

	//Find color of lower right
	color = float(s) / 3.0;
	fragColor = vec3(color, color, color);
	
	EmitVertex();
	gl_Position = position;
	EmitVertex();
	gl_Position = brLPosition;
	EmitVertex();

	EndPrimitive();
}