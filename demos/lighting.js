import PicoGL from "../node_modules/picogl/build/module/picogl.js";
import {mat4, vec3} from "../node_modules/gl-matrix/esm/index.js";

import {positions, normals, indices} from "../blender/monkey.js"

// ******************************************************
// **               Light configuration                **
// ******************************************************

let baseColor = vec3.fromValues(1.0, 0.1, 0.2);
let ambientLightColor = vec3.fromValues(0.1, 0.1, 1.0);
let numberOfPointLights = 2;
let pointLightColors = [vec3.fromValues(1.0, 1.0, 1.0), vec3.fromValues(0.02, 0.4, 0.5)];
let pointLightInitialPositions = [vec3.fromValues(5, 0, 2), vec3.fromValues(-5, 0, 2)];
let pointLightPositions = [vec3.create(), vec3.create()];


// language=GLSL
let lightCalculationShader = `
    uniform vec3 cameraPosition;
    uniform vec3 baseColor;    

    uniform vec3 ambientLightColor;    
    uniform vec3 lightColors[${numberOfPointLights}];        
    uniform vec3 lightPositions[${numberOfPointLights}];
    
    // This function calculates light reflection using Phong reflection model (ambient + diffuse + specular)
    vec4 calculateLights(vec3 normal, vec3 position) {
        float ambientIntensity = 0.5;
        float diffuseIntensity = 1.0;
        float specularIntensity = 2.0;
        float specularPower = 100.0;
        float metalness = 0.0;

        vec3 viewDirection = normalize(cameraPosition.xyz - position);
        vec3 color = baseColor * ambientLightColor * ambientIntensity;
                
        for (int i = 0; i < lightPositions.length(); i++) {
            vec3 lightDirection = normalize(lightPositions[i] - position);
            
            // Lambertian reflection (ideal diffuse of matte surfaces) is also a part of Phong model                        
            float diffuse = max(dot(lightDirection, normal), 0.0);                                    
            color += baseColor * lightColors[i] * diffuse * diffuseIntensity;
                      
            // Phong specular highlight 
            float specular = pow(max(dot(viewDirection, reflect(-lightDirection, normal)), 0.0), specularPower);
            
            // Blinn-Phong improved specular highlight
            // float specular = pow(max(dot(normalize(lightDirection + viewDirection), normal), 0.0), specularPower);
            color += mix(vec3(1.0), baseColor, metalness) * lightColors[i] * specular * specularIntensity;
        }
        return vec4(color, 1.0);
    }
`;

// language=GLSL
let fragmentShader = `
    #version 300 es
    precision highp float;        
    ${lightCalculationShader}        
    
    in vec3 vPosition;    
    in vec3 vNormal;
    in vec4 vColor;    
    
    out vec4 outColor;        
    
    void main() {                      
        // For Phong shading (per-fragment) move color calculation from vertex to fragment shader
        outColor = calculateLights(normalize(vNormal), vPosition);
        // outColor = vColor;
    }
`;

// language=GLSL
let vertexShader = `
    #version 300 es
    ${lightCalculationShader}
        
    layout(location=0) in vec4 position;
    layout(location=1) in vec4 normal;
    
    uniform mat4 viewProjectionMatrix;
    uniform mat4 modelMatrix;            
    
    out vec3 vPosition;    
    out vec3 vNormal;
    out vec4 vColor;
    
    void main() {
        vec4 worldPosition = modelMatrix * position;
        
        vPosition = worldPosition.xyz;        
        vNormal = (modelMatrix * normal).xyz;
        
        // For Gouraud shading (per-vertex) move color calculation from fragment to vertex shader
        //vColor = calculateLights(normalize(vNormal), vPosition);
        
        gl_Position = viewProjectionMatrix * worldPosition;                        
    }
`;


app.enable(PicoGL.DEPTH_TEST)
   .enable(PicoGL.CULL_FACE);

let program = app.createProgram(vertexShader.trim(), fragmentShader.trim());

let vertexArray = app.createVertexArray()
    .vertexAttributeBuffer(0, app.createVertexBuffer(PicoGL.FLOAT, 3, positions))
    .vertexAttributeBuffer(1, app.createVertexBuffer(PicoGL.FLOAT, 3, normals))
    .indexBuffer(app.createIndexBuffer(PicoGL.UNSIGNED_INT, 3, indices));

let projectionMatrix = mat4.create();
let viewMatrix = mat4.create();
let viewProjectionMatrix = mat4.create();
let modelMatrix = mat4.create();

let drawCall = app.createDrawCall(program, vertexArray)
    .uniform("baseColor", baseColor)
    .uniform("ambientLightColor", ambientLightColor);

let cameraPosition = vec3.fromValues(0, 0, 4);
mat4.fromXRotation(modelMatrix, -Math.PI / 2);

const positionsBuffer = new Float32Array(numberOfPointLights * 3);
const colorsBuffer = new Float32Array(numberOfPointLights * 3);

function draw(timestamp) {
    let time = timestamp / 1000;

    mat4.perspective(projectionMatrix, Math.PI / 4, app.width / app.height, 0.1, 100.0);
    mat4.lookAt(viewMatrix, cameraPosition, vec3.fromValues(0, 0, 0), vec3.fromValues(0, 1, 0));
    mat4.multiply(viewProjectionMatrix, projectionMatrix, viewMatrix);

    drawCall.uniform("viewProjectionMatrix", viewProjectionMatrix);
    drawCall.uniform("modelMatrix", modelMatrix);
    drawCall.uniform("cameraPosition", cameraPosition);

    for (let i = 0; i < numberOfPointLights; i++) {
        vec3.rotateZ(pointLightPositions[i], pointLightInitialPositions[i], vec3.fromValues(0, 0, 0), time);
        positionsBuffer.set(pointLightPositions[i], i * 3);
        colorsBuffer.set(pointLightColors[i], i * 3);
    }

    drawCall.uniform("lightPositions[0]", positionsBuffer);
    drawCall.uniform("lightColors[0]", colorsBuffer);

    app.clear();
    drawCall.draw();

    requestAnimationFrame(draw);
}
requestAnimationFrame(draw);
