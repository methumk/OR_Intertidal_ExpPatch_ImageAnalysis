from os.path import dirname, abspath, join
from unittest.mock import patch
import torch
import torchvision
from torch.utils.data import DataLoader

import torch.nn as nn
import torch.optim as optim
from torch.optim import lr_scheduler
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import torch
from PIL import Image
from torchvision import transforms, models, datasets, utils
import os
import cv2
import time
import copy
from collections import Counter

from dataset import (
    PatchPicsDataset,
)

def tile_image(split_factor, img_file, classifications, data_type="train"):
    im =  cv2.imread(img_file)
    print("IM SHAPE: ",im.shape)
    # im = cv2.resize(im,(1000,500))
    img_date = img_file.split("\\")[-1].split(".")[0]
    print("IMG DATE:", img_date)
    imgheight=im.shape[0]
    imgwidth=im.shape[1]

    y1 = 0
    M = imgheight//split_factor
    N = imgwidth//split_factor

    for y in range(0,imgheight,M):
        for x in range(0, imgwidth, N):
            y1 = y + M
            x1 = x + N
            tiles = im[y:y+M,x:x+N]
            
            classification = Counter()
            for key, value in classifications.items():
                coordinates_for_class = classifications[key]
                for coordinate in coordinates_for_class:
                    if int(coordinate[0]) < x or int(coordinate[0]) > x1:
                        break
                    if int(coordinate[0]) <= x1 and int(coordinate[0]) >= x:
                        classification[key] += 1
            top_class = classification.most_common(1)[0][0] if classification else "None"
            


            cv2.rectangle(im, (x, y), (x1, y1), (0, 255, 0))

                
            if not os.path.exists(f"{data_type}/{str(top_class)}"):
                os.makedirs(f"{data_type}/{str(top_class)}/")

            cv2.imwrite(os.path.join(f"{data_type}/{str(top_class)}",f"{img_date}_{str(x)}-{str(x1)}_{str(y)}-{str(y1)}.png"),tiles)
    cv2.imwrite(f"grid-{img_file}",im)

def classification(classifications):
    classification = Counter()
    for key, value in classifications.items():
        coordinates_for_class = classifications[key]
        for coordinate in coordinates_for_class:
            if int(coordinate[0]) <= x1 and int(coordinate[0]) >= x:
                classification[key] += 1
    top_class = classification.most_common(1)[0][0] if classification else "None"

def imshow(inp, title=None):
    """Imshow for Tensor."""
    print("TITLE: ", title)

    inp = inp.numpy().transpose((1, 2, 0))
    mean = np.array([0.485, 0.456, 0.406])
    std = np.array([0.229, 0.224, 0.225])
    inp = std * inp + mean
    inp = np.clip(inp, 0, 1)
    plt.imshow(inp)
    if title is not None:
        plt.title(title)
    plt.pause(0.001)  # pause a bit so that plots are updated
    plt.show()


def visualize_model(model, dataloaders, device, class_names, num_images=6):
    was_training = model.training
    model.eval()
    images_so_far = 0
    fig = plt.figure()

    with torch.no_grad():
        for i, (inputs, labels) in enumerate(dataloaders['val']):
            inputs = inputs.to(device)
            labels = labels.to(device)

            outputs = model(inputs)
            _, preds = torch.max(outputs, 1)

            for j in range(inputs.size()[0]):
                images_so_far += 1
                ax = plt.subplot(num_images//2, 2, images_so_far)
                ax.axis('off')
                ax.set_title('predicted: {}'.format(class_names[preds[j]]))
                imshow(inputs.cpu().data[j])

                if images_so_far == num_images:
                    model.train(mode=was_training)
                    return
        model.train(mode=was_training)

def train_model(model, criterion, optimizer, scheduler, dataloaders, device, dataset_sizes, num_epochs=25):
    since = time.time()

    best_model_wts = copy.deepcopy(model.state_dict())
    best_acc = 0.0

    for epoch in range(num_epochs):
        print('Epoch {}/{}'.format(epoch, num_epochs - 1))
        print('-' * 10)

        # Each epoch has a training and validation phase
        for phase in ['train', 'val']:
            if phase == 'train':
                model.train()  # Set model to training mode
            else:
                model.eval()   # Set model to evaluate mode

            running_loss = 0.0
            running_corrects = 0

            # Iterate over data.
            for inputs, labels in dataloaders[phase]:
                inputs = inputs.to(device)
                labels = labels.to(device)

                # zero the parameter gradients
                optimizer.zero_grad()

                # forward
                # track history if only in train
                with torch.set_grad_enabled(phase == 'train'):
                    outputs = model(inputs)
                    _, preds = torch.max(outputs, 1)
                    loss = criterion(outputs, labels)

                    # backward + optimize only if in training phase
                    if phase == 'train':
                        loss.backward()
                        optimizer.step()

                # statistics
                running_loss += loss.item() * inputs.size(0)
                running_corrects += torch.sum(preds == labels.data)
            if phase == 'train':
                scheduler.step()

            epoch_loss = running_loss / dataset_sizes[phase]
            epoch_acc = running_corrects.double() / dataset_sizes[phase]

            print('{} Loss: {:.4f} Acc: {:.4f}'.format(
                phase, epoch_loss, epoch_acc))

            # deep copy the model
            if phase == 'val' and epoch_acc > best_acc:
                best_acc = epoch_acc
                best_model_wts = copy.deepcopy(model.state_dict())

        print()

    time_elapsed = time.time() - since
    print('Training complete in {:.0f}m {:.0f}s'.format(
        time_elapsed // 60, time_elapsed % 60))
    print('Best val Acc: {:4f}'.format(best_acc))

    # load best model weights
    model.load_state_dict(best_model_wts)
    return model

def test_patch_dataset():
    patch_path = "ExpPatch-Pics"
    batch_size = 10

    # Get absolute path to `ExpPatch-Pics` directory
    abs_path = dirname(dirname(dirname(abspath(__file__))))
    patch_path = join(abs_path, patch_path)

    patch_pic_dataset = PatchPicsDataset(dataset_path=patch_path)
    # patch_pic_dataset.open_image()
    # patch_pic_dataset.to_file()
    # patch_pic_dataset.patches_dict_to_file()
    print("SHAPE OF ENTIRE PICS ARRAY\n\n\n\n: ", patch_pic_dataset)
    img_file1 = patch_pic_dataset[0][2]
    print(img_file1)

    get_next = iter(patch_pic_dataset)
    counter = 0
    try:
        next(get_next)
        while True:
            class_and_file_path = next(get_next)
            labeling = patch_pic_dataset[0][1]
            print("tiling file: ", class_and_file_path[2])
            for key, value in labeling.items():
                labeling[key] = sorted(labeling[key])
            data_type = "train" if counter <= 30 else "val"
            print("data type: ", data_type)
            tile_image(split_factor=20, img_file=class_and_file_path[2], classifications=labeling, data_type=data_type)
            counter+=1
    except StopIteration:
        print("Stop iteration")
    print("COUNTER: ", counter)


    

    # dict = patch_pic_dataset[0][1]
    # print("FIRST FILE: ", img_file1)
    # for key, value in dict.items():
    #     dict[key] = sorted(dict[key])
    # tile_image(split_factor=20, img_file=img_file1, classifications=dict, data_type="train")


    # img_file2 = patch_pic_dataset[1][2]
    # print("SECOND FILE: ", img_file2)
    # dict = patch_pic_dataset[1][1]
    # for key, value in dict.items():
    #     dict[key] = sorted(dict[key])
    # tile_image(split_factor=20, img_file=img_file2, classifications=dict, data_type="val")
    
    # return
    
    # does the tiling process require the MxNx3 rgb nparray to be split, or do I split the tensors?
    # is the rgb image representation from imread() or the tensor correct?

    # https://bowenroom.github.io/myBlog/pytorch/fastai2/2020/06/01/torch-unfold.html#pytorch-unfold-&-fold
    # read further to implement unfolding; potential solution to tiling





    # https://pytorch.org/tutorials/beginner/finetuning_torchvision_models_tutorial.html#Resnet
    # read further to use resnet for transfer learing?

    # https://pytorch.org/hub/pytorch_vision_resnet/
    # print(img_file)
    # input_image = Image.open(img_file)
    preprocess = {
    'train': transforms.Compose([
        transforms.RandomResizedCrop(224),
        transforms.RandomHorizontalFlip(),
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
    ]),
    'val': transforms.Compose([
        transforms.Resize(256),
        transforms.CenterCrop(224),
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
    ]),
    }



    image_datasets = {x: datasets.ImageFolder(x,
                                            preprocess[x])
                    for x in ['train', 'val']}
    dataloaders = {x: torch.utils.data.DataLoader(image_datasets[x], batch_size=4,
                                                shuffle=True, num_workers=4)
                for x in ['train', 'val']}
    dataset_sizes = {x: len(image_datasets[x]) for x in ['train', 'val']}
    class_names = image_datasets['train'].classes
    print("CLASS NAMES: ", class_names)
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")



    # Get a batch of training data
    inputs, classes = next(iter(dataloaders['train']))
    # print("INPUTS AND CLASSES: ", inputs, classes)
    # Make a grid from batch
    out = torchvision.utils.make_grid(inputs)
    # matplotlib.get_backend()
    # imshow(out, title=[class_names[x] for x in classes])


    model_ft = models.resnet50(pretrained=True)
    num_ftrs = model_ft.fc.in_features
    # Here the size of each output sample is set to 2.
    # Alternatively, it can be generalized to nn.Linear(num_ftrs, len(class_names)).
    model_ft.fc = nn.Linear(num_ftrs, len(class_names))

    model_ft = model_ft.to(device)

    criterion = nn.CrossEntropyLoss()

    # Observe that all parameters are being optimized
    optimizer_ft = optim.SGD(model_ft.parameters(), lr=0.001, momentum=0.9)

    # Decay LR by a factor of 0.1 every 7 epochs
    exp_lr_scheduler = lr_scheduler.StepLR(optimizer_ft, step_size=7, gamma=0.1)


    model_ft = train_model(model_ft, criterion, optimizer_ft, exp_lr_scheduler, dataloaders, device, dataset_sizes,
                        num_epochs=25)

    visualize_model(model_ft, dataloaders, device, class_names)


    # dataloader = DataLoader(
    #     dataset=patch_pic_dataset,
    #     batch_size=batch_size,
    #     shuffle=True,
    #     num_workers=0,
    # )
    # print(f'Returned value: {test_return_value}')

    # https://www.kaggle.com/pmigdal/transfer-learning-with-resnet-50-in-pytorch
  
if __name__ == "__main__":
    test_patch_dataset()