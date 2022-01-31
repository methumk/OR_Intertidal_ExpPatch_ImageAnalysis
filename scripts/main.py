import torchvision.transforms as transforms
import torchvision.models as models
import torch.optim as optim
import logging
import torch

from dataloader.dataset import PatchPicsDataset
from os.path import dirname, abspath, join
from trainer.train import Trainer
from torch.utils.data.dataset import Dataset


logging.basicConfig(
    format="%(asctime)s [%(name)s: %(levelname)s] | %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.INFO,
)

logger = logging.getLogger("main.py")

lr = 0.01
batch_size = 8


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
        train_set, batch_size=batch_size, shuffle=True, num_workers=0,
    )

    print(train_set[0])


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


if __name__ == "__main__":
    main()
