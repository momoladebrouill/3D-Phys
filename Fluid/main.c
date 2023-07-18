#include "raylib.h"

#define N 10
#define IX(i,j) ((i)+(N+2)*(j))

const int WIDTH = 500;
const int HEIGHT= 500;



void add_source(int size,float *x, float *s, float dt){
    for(int i=0;i<size;i++){
        x[i] += dt*s[i];
    }
}
/*
   void diffuse ( int N, int b, float * x, float * x0, float diff, float dt )
   {
   int i, j, k;
   float a=dt*diff*N*N;
   for ( k=0 ; k<20 ; k++ ) {
   for ( i=1 ; i<=N ; i++ ) {
   for ( j=1 ; j<=N ; j++ ) {
   x[IX(i,j)] = (x0[IX(i,j)] + a*(x[IX(i-1,j)]+x[IX(i+1,j)]+

   x[IX(i,j-1)]+x[IX(i,j+1)]))/(1+4*a);

   }
   }
   set_bnd ( N, b, x );
   }
   }*/

int main(void){
    int const size = (N+2)*(N+2);
    int u[size],v[size],u_prev[size],v_prev[size];
    int dens[size], dens_prev[size];
    InitWindow(WIDTH,HEIGHT,"courage");
    SetTargetFPS(60);
    Color c = {0};
    while(!WindowShouldClose()){
        BeginDrawing();
        ClearBackground(RAYWHITE);
        c = (Color){ 255, 0, 0, GetRandomValue(0,255) };
        DrawRectangle(10,10,10,10,c);
        EndDrawing();
    }
    CloseWindow();
}
