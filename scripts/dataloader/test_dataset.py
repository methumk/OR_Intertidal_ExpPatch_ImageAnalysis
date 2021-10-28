from os.path import dirname, abspath, join

from OR_Intertidal_ExpPatch_ImageAnalysis.scripts.dataloader.dataset import (
    PatchPicsDataset,
)


def test_patch_dataset():
    patch_path = "ExpPatch-Pics"

    # Get absolute path to `ExpPatch-Pics` directory
    abs_path = dirname(dirname(dirname(abspath(__file__))))
    patch_path = join(abs_path, patch_path)

    patch_pic_dataset = PatchPicsDataset(dataset_path=patch_path)
    # patch_pic_dataset.patches_dict_to_file()


if __name__ == "__main__":
    test_patch_dataset()
