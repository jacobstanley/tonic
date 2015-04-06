// letrec f = \x ->
//     letrec g = \y ->
//         letrec h = \z ->
//             if (z == 0)
//             then true
//             else z1 = z - 1
//                  f z1
//         h y
//     g x
// f 10

////////////////////////////////////////////////////////////////////////

boolean main() {
    f_x = 10;
    goto f;

f:
    g_y = f_x;
    goto g;

g:
    h_z = g_y;
    goto h;

h:
    if (h_z == 0) {
        return true;
    } else {
        z1 = h_z - 1;

        f_x = z1;
        goto f;
    }
}
