#version 400
layout(location = 0) in vec2 vp;
layout(location = 1) in vec3 vertexColor;
layout(location = 2) in uint vertexStrength;
out vec3 geoColor;
out uint strength;
void main() {
	gl_Position = vec4(vp, 0.0, 1.0);
	geoColor = vertexColor;
	strength = vertexStrength;
}
