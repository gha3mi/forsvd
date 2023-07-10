from PIL import Image
import numpy as np

image_array = np.loadtxt('pixel/John_W_Backus_com.txt')
image_array = image_array.astype(np.uint8)
image       = Image.fromarray(image_array)
image.save('pixel/John_W_Backus_com.jpg')
