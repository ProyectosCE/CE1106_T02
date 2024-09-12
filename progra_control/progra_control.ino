#include <Keyboard.h>  // biblioteca para emular teclas

// Define joystick pins
const int joyXPin = A0;
const int joyYPin = A1;
const int buttonPin = 2;  

const int threshold = 300;

// 
int lastXState = 0;
int lastYState = 0;
bool buttonPressed = false;

void setup() {
  // inicializar los pines
  pinMode(buttonPin, INPUT_PULLUP);
  
  
  Keyboard.begin();
}

void loop() {
  int joyX = analogRead(joyXPin);  // lee eje x
  int joyY = analogRead(joyYPin);  // lee el eje y
  int buttonState = digitalRead(buttonPin);  // lee los botones
  
  // emulas las wasd
  if (joyX < threshold && lastXState != -1) {  // izquierda (A)
    Keyboard.press('a');
    lastXState = -1;
  } else if (joyX > 1023 - threshold && lastXState != 1) {  // derecha (D)
    Keyboard.press('d');
    lastXState = 1;
  } else if (joyX >= threshold && joyX <= 1023 - threshold && lastXState != 0) {  // suelta A/D
    Keyboard.release('a');
    Keyboard.release('d');
    lastXState = 0;
  }

  if (joyY < threshold && lastYState != 1) {  // arriba (W)
    Keyboard.press('w');
    lastYState = 1;
  } else if (joyY > 1023 - threshold && lastYState != -1) {  // abajo (S)
    Keyboard.press('s');
    lastYState = -1;
  } else if (joyY >= threshold && joyY <= 1023 - threshold && lastYState != 0) {  // suelta W/S
    Keyboard.release('w');
    Keyboard.release('s');
    lastYState = 0;
  }

  // emula el espacio 
  if (buttonState == LOW && !buttonPressed) {
    Keyboard.press(' ');
    buttonPressed = true;
  } else if (buttonState == HIGH && buttonPressed) {
    Keyboard.release(' ');
    buttonPressed = false;
  }

  delay(50);  // evita el acumulacion
}
