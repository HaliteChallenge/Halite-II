#version 400
layout(location = 0) in vec2 vp;
layout(location = 1) in float vertexColor;
out vec3 geoColor;
void main() {
	gl_Position = vec4(vp, 0.0, 1.0);
	geoColor = vec3(vertexColor, vertexColor, vertexColor);
}
