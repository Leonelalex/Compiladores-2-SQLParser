import tkinter

ventana = tkinter.Tk()
ventana.geometry("500*500")

texto = tkinter.Label(ventana, text = 'Mi primer texto')
texto.pack()

ventana.mainloop()