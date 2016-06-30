#version 400
layout(location = 0) in vec2 vp;
layout(location = 1) in float vertexColor;
out vec3 geoColor;
void main() {
	gl_Position = vec4(vp, 0.0, 1.0);
	//Interpolate this float into a spectrum.
	float f = pow(vertexColor, 0.5) * 2;
	if(f < 1) geoColor = vec3(f, 1, 0);
	else if(f < 2) geoColor = vec3(1, 2 - f, 0);
	else geoColor = vec3(1, 0, 0);
}
