#version 330
uniform float width;
uniform float height;
layout(points) in;
layout(triangle_strip, max_vertices = 16) out;
in vec3 geoColor[];
in uint strength[];
out vec3 color;
void main() {
	vec4 position = gl_in[0].gl_Position;
	color = geoColor[0];

	//Find top left vertices.
	vec4 tlBPosition = position + vec4(-width, height, 0.0, 0.0);

	//Find bottom left vertices.
	vec4 blBPosition = position + vec4(-width, -height, 0.0, 0.0);

	//Find top right vertices.
	vec4 trBPosition = position + vec4(width, height, 0.0, 0.0);

	//Find bottom right vertices.
	vec4 brBPosition = position + vec4(width, -height, 0.0, 0.0);

	//Create outer square:
	gl_Position = tlBPosition;
	EmitVertex();
	gl_Position = trBPosition;
	EmitVertex();
	gl_Position = blBPosition;
	EmitVertex();
	gl_Position = brBPosition;
	EmitVertex();

	EndPrimitive();
}
