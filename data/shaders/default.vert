#version 410 core

layout (location = 0) in vec3 s_position;
layout (location = 3) in vec2 s_uv;

out vec3 f_position;
out vec2 f_uv;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

/*
    Calculates the perspective projection of the given vertex-position vector
    w.r.t the given view-model transform and perspective parameters.

    Passes the given UV coordinates unchanged to the fragment shader.
*/
void main()
{
    mat4 modelView = view * model;

    gl_Position = projection * modelView * vec4(s_position, 1.0);
    f_position = (modelView * vec4(s_position, 1.0)).xyz;
    f_uv = s_uv;
}
