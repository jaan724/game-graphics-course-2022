import PicoGL from "../node_modules/picogl/build/module/picogl.js";
import {mat4, vec3, vec4} from "../node_modules/gl-matrix/esm/index.js";

function draw(timems) {
    let time = timems / 1000;

    requestAnimationFrame(draw);
}
requestAnimationFrame(draw);
