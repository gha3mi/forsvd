from PIL import Image
import numpy as np

image       = Image.open('pixel/John_W_Backus.jpg')
image       = image.convert('L')
image_array = np.array(image)
np.savetxt('pixel/John_W_Backus.txt', image_array, fmt='%d')