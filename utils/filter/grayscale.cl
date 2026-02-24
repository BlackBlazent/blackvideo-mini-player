// grayscale.cl
__kernel void grayscale(__global uchar4* frame, int width, int height) {
    int i = get_global_id(0);
    uchar4 pixel = frame[i];
    uchar gray = (uchar)(0.299f*pixel.x + 0.587f*pixel.y + 0.114f*pixel.z);
    frame[i] = (uchar4)(gray, gray, gray, pixel.w);
}