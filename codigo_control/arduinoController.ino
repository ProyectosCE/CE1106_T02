#include <Keyboard.h>  // Biblioteca para emular teclas en la computadora

// Definir pines del joystick
const int joyXPin = A0;  // Pin analógico para el eje X del joystick
const int joyYPin = A1;  // Pin analógico para el eje Y del joystick
const int buttonPin = 2;  // Pin digital para el botón del joystick

const int threshold = 300;  // Umbral para detectar movimiento en los ejes

// Variables para almacenar el estado anterior del joystick
int lastXState = 0;  
int lastYState = 0;  
bool buttonPressed = false;  

void setup() {
  // Inicializar el pin del botón como entrada con resistencia pull-up interna
  pinMode(buttonPin, INPUT_PULLUP);
  
  // Inicializar la funcionalidad de teclado
  Keyboard.begin();
}

void loop() {
  // Leer los valores del joystick en los ejes X e Y
  int joyX = analogRead(joyXPin);  
  int joyY = analogRead(joyYPin);  
  int buttonState = digitalRead(buttonPin);  
  
  // Manejar el movimiento horizontal (eje X) para emular las teclas 'a' (izquierda) y 'd' (derecha)
  if (joyX < threshold && lastXState != -1) {  // Si el joystick está a la izquierda y no estaba ya en esa posición
    Keyboard.press('a');  
    lastXState = -1;  
  } else if (joyX > 1023 - threshold && lastXState != 1) {  // Si el joystick está a la derecha
    Keyboard.press('d');  
    lastXState = 1;  
  } else if (joyX >= threshold && joyX <= 1023 - threshold && lastXState != 0) {  // Si el joystick está en el centro
    Keyboard.release('a');  
    Keyboard.release('d');  
    lastXState = 0;  
  }

  // Manejar el movimiento vertical (eje Y) para emular las teclas 'w' (arriba) y 's' (abajo)
  if (joyY < threshold && lastYState != 1) {  // Si el joystick está hacia arriba
    Keyboard.press('w');  
    lastYState = 1;  
  } else if (joyY > 1023 - threshold && lastYState != -1) {  // Si el joystick está hacia abajo
    Keyboard.press('s');  
    lastYState = -1;  
  } else if (joyY >= threshold && joyY <= 1023 - threshold && lastYState != 0) {  // Si el joystick está en el centro
    Keyboard.release('w');  
    Keyboard.release('s');  
    lastYState = 0;  
  }

  // Manejar el botón del joystick para emular la barra espaciadora
  if (buttonState == LOW && !buttonPressed) {  // Si el botón se presiona
    Keyboard.press(' ');  // Presionar la barra espaciadora
    buttonPressed = true;  // Actualizar el estado del botón a presionado
  } else if (buttonState == HIGH && buttonPressed) {  // Si el botón se suelta
    Keyboard.release(' ');  // Soltar la barra espaciadora
    buttonPressed = false;  // Actualizar el estado del botón a no presionado
  }

  // Esperar un corto tiempo para evitar múltiples lecturas rápidas del joystick
  delay(50); 
}
