from os.path import dirname, abspath, join
from torch.utils.data import DataLoader

from dataset import (
    PatchPicsDataset,
    convert_labels_to_yolo,
)


def test_patch_dataset():
    patch_path = "ExpPatch-Pics"
    batch_size = 10

    # Test ground truth bbox
    G = {i: (100.0, 100.0) for i in range(1, 26)}

    # Get absolute path to `ExpPatch-Pics` directory
    abs_path = dirname(dirname(dirname(abspath(__file__))))
    patch_path = join(abs_path, patch_path)

    patch_pic_dataset = PatchPicsDataset(dataset_path=patch_path)

    # Convert labels to YOLOv5 format
    x, y = patch_pic_dataset[0]
    bboxes = convert_labels_to_yolo(x, y, G)

    exit(420)

    # Dataloader object
    dataloader = DataLoader(
        dataset=patch_pic_dataset,
        batch_size=batch_size,
        shuffle=False,
        num_workers=0,
    )


if __name__ == "__main__":
    test_patch_dataset()
