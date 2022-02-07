import torchvision.transforms as transforms
import torchvision.models as models
import torch.optim as optim
import logging
import torch
from sklearn.neural_network import MLPClassifier

from dataloader.dataset import PatchPicsDataset
from os.path import dirname, abspath, join
from trainer.train import Trainer
from torch.utils.data.dataset import Dataset

from PIL import Image

logging.basicConfig(
    format="%(asctime)s [%(name)s: %(levelname)s] | %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.INFO,
)

logger = logging.getLogger("main.py")
 #for displaying image
img_transform = transforms.ToPILImage()
#add following lines for when you want to display an image
#im = img_transform(batch[0])
#im.show()

lr = 0.01
batch_size = 1


def device_selector():
    """Select what device to run training on.
    Uses cuda if available.

    Returns: (torch.device) Device the training is going to be run on.
    """
    use_cuda = torch.cuda.is_available()
    if use_cuda:
        logger.info(
            "CUDA version: [ %s ] | CUDA DEVICE: [ %s ]",
            torch.version.cuda,
            torch.cuda.get_device_name(torch.cuda.current_device()),
        )
    return torch.device("cuda" if use_cuda else "cpu")


def windowed_collate_fn(batch):
    w, h = 256, 256
    # Batch: (image, dict of labels)
    output_batch = []
    windowed_imgs, windowed_labels = [], []
    print("collate_fn:")
    #print(batch[1])
    #im = img_transform(batch[0])
    #im.show()
    #print(type(batch[0]), type(batch[1]))
    for data, labels in batch:
        for label, center_pt in labels.items():
            # Skip empty label
            if not center_pt:
                continue
            for x, y in center_pt:
                windowed_img = data[:, x-w:x+w, y-h:y+h]#.tolist()
                windowed_imgs.append((windowed_img))
                windowed_labels.append((label))
                output_batch.append([(windowed_img), (label)])
    #winddowed_imgs and windowed_labels are tensors
    print(f'wind imgs: {type(windowed_imgs[0])}, wind lbls : {type(windowed_labels)}')
    return windowed_imgs, windowed_labels


def main():
    device = device_selector()

    transform = transforms.Compose(
        [transforms.ToTensor()]
    )

    # Get absolute path to `ExpPatch-Pics` directory
    abs_path = dirname(dirname(abspath(__file__)))
    patch_path = join(abs_path, "ExpPatch-Pics")

    train_set = PatchPicsDataset(dataset_path=patch_path, transform=transform)
    # test_set = PatchPicsDataset(dataset_path="wherever test data is")

    trainloader = torch.utils.data.DataLoader(
        train_set, batch_size=batch_size, shuffle=True, num_workers=0, collate_fn=windowed_collate_fn,
    )

    #t = windowed_collate_fn(train_set[0])
    # print(type(train_set[0][0]), type(train_set[0][1]))
    # print(f"shape trainset[0]: {train_set[0][0].shape}")
    # print(f'train set[0]:\n{train_set[0][0]}\n-----------------------------------------')
    # print(f'train set[1]:\n{train_set[0][1]}\n-----------------------------------------')


    # testloader = torch.utils.data.DataLoader(
    #     test_set, batch_size=batch_size, shuffle=True, num_workers=0
    # )

    model = models.resnet50(pretrained=True)
    loss_fn = torch.nn.CrossEntropyLoss()
    optimizer = optim.SGD(model.parameters(), lr=lr)

    trainer = Trainer(
        data_loader=trainloader,
        model=model,
        optimizer=optimizer,
        loss_fn=loss_fn,
        device=device,
    )
    trainer.fit_model(1)

    #use mlp classifier on dataset consisting of (image, label, feature vector)
    mlp = MLPClassifier(solver='lbfgs', hidden_layer_sizes=50,
                                max_iter=150, shuffle=True, random_state=1) 
    # X = input data i.e. image and feature vector, y = target values i.e. labels 
    #mlp.fit(X, y)
    #make predictions
    #mlp.predict(X)


if __name__ == "__main__":
    main()
