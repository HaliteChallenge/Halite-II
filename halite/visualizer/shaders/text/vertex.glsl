#version 330
layout(location = 0) in vec2 vertexPosition;
layout(location = 1) in vec2 vertexUV;
out vec2 UV;
void main() {
	gl_Position = vec4(vertexPosition, 0, 1);
	UV = vertexUV;
}
