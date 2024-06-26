from Manager import Manager
import tkinter as tk


def on_button_click():
    label.config(text="Hola jefferson \n holaa")

root = tk.Tk()
root.title("Scrum US Distribution")

root.geometry("650x300")

label = tk.Label(root, text="Presiona el botón")
label.pack(pady=10)

button = tk.Button(root, text="Presionar", command=on_button_click)
button.pack(pady=10)

root.mainloop()