from abc import abstractmethod, abstractproperty
from collections import namedtuple
import os
from os.path import dirname, abspath, join
import re
import logging
import torch
from typing import Any, Callable, Dict, List, Optional, Union
from torch.utils.data import Dataset
from torchvision import transforms, utils

logging.basicConfig(
    format="%(asctime)s [%(name)s: %(levelname)s] | %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.INFO,
)

logger = logging.getLogger("dataset.py")


class PatchDataset(Dataset):
    """Dataset class for patch data."""

    _PATCH_ID = [
        "A",
        "B",
        "C",
        "D",
        "E",
        "F",
        "G",
        "AB",
        "AC",
        "AD",
        "AE",
        "AF",
        "AG",
        "B",
        "BB",
        "BC",
        "BD",
        "BE",
        "BF",
    ]
    # Supported date formats: YYYY-MM-DD, YYYY_MM_DD
    _DATE_FORMAT = "([0-9]{4}\-[0-9]{2}\-[0-9]{2})|([0-9]{4}\_[0-9]{2}\_[0-9]{2})"
    _SAMPLE = namedtuple("Sample", "file date")

    def __init__(
        self,
        dataset_path: Union[os.PathLike, str],
        sample_mapping: List[namedtuple] = None,
        transform: Optional[Callable] = None,
    ):
        """init method.

        Args:
            sample_mapping: patches dict that has been extracted from file/previous save.
            dataset_path: directory containing all of the files that contain the samples.
            transform: Optional transform to be applied on a sample.
        """
        self._samples = sample_mapping
        self._dataset_path = dataset_path
        self._transform = transform

        self._abs_path = dirname(dirname(dirname(abspath(__file__))))

        # Create a mapping from PATCH_ID to list of samples in the patch.
        self._patches = {key: [] for key in self._PATCH_ID}

    def _check_attr(self, attr_name: str, check_val_exists: bool = False):
        if not hasattr(self, attr_name):
            raise RuntimeError(
                f"{type(self).__name__} needs to call super().__init__() before {attr_name} can be used."
            )
        if check_val_exists:
            if getattr(self, attr_name) is None:
                raise RuntimeError(f"{attr_name} is not initialized")

    def __len__(self) -> int:
        sum_lenghts = 0
        for v in self.patches.values:
            sum_lenghts += len(v)

        return sum_lenghts

    @property
    def patches(self) -> Dict[str, List[namedtuple]]:
        """patches property."""
        self._check_attr("_patches", check_val_exists=True)
        for k, v in self._patches.items():
            if not v:
                raise RuntimeError(
                    f"{type(self).__name__}._patches[{k}] has not been assigned any values!"
                )

        return self._patches

    @patches.setter
    def patches(self, patches: Dict[str, namedtuple]):
        """patches setter.

        Args:
            patches (list or namedtuple): containing a list of or a single sample.
        """
        self._check_attr("_patches")
        for k, sample in patches.items():
            if k in self._patches.keys():
                if isinstance(patches, list):
                    self._patches[k].extend(sample)
                else:
                    self._patches[k].append(sample)
            else:
                logger.warning("%s is not a valid PATCH_ID", str(k))

    def _search_for_file(self, **kwargs):
        """Generate a mapping from a given index to a relative file path.

        Args:
            file_type: file type of files that will be added to dataset.
        """
        self._check_attr("_dataset_path", check_val_exists=True)
        for root, dirs, files in os.walk(self._dataset_path, topdown=True):
            for name in files:
                self._map_files_to_patches(
                    name=name,
                    root=root,
                    dirs=dirs,
                    files=files,
                    kwargs=kwargs
                )

    @abstractmethod
    def _map_files_to_patches(self, **kwargs):
        """Method that defines a policy for filling patches"""
        pass

    def patches_dict_to_file(self):
        """Temp method for testing.
        A better file output will be implemented later.
        """
        with open(r"test_output.txt", "w") as f:
            # yaml.dump(self._patches, f)
            for k, v in self._patches.items():
                print(str(f"{k}:"), file=f)
                for sample in v:
                    print(str(f"{sample}"), file=f)

    @abstractmethod
    def __getitem__(self, idx):
        """getitem method."""
        pass


class PatchPicsDataset(PatchDataset):
    """Dataset class for .jpg images"""

    _FILE_TYPE = ".jpg"

    def __init__(
        self,
        dataset_path: Union[os.PathLike, str],
        sample_mapping: List[namedtuple] = None,
        transform: Optional[Callable] = None,
    ):
        super().__init__(dataset_path, sample_mapping, transform)

        self._search_for_file()

    def _map_files_to_patches(self, root, dirs, files, name, **kwargs):
        # TODO: add something to deal with `cropped/` photos

        if re.search(self._FILE_TYPE, name):
            print(name)
            rel_path = "".join(root.split(self._abs_path))
            date = re.search(self._DATE_FORMAT, name)
            if not date:
                logger.warning("No date found in: %s", str(name))
                return
            patch_id = re.findall("[A-Z]-(.+?)q", name)
            if patch_id:
                self.patches = {
                    str(patch_id[0]): self._SAMPLE(
                        join(rel_path, name), date.group(1)
                    ),
                }

    def __getitem__(self, idx):
        """getitem method.

        Args:
            idx (int or torch.tensor): index of sample in the dataset.
        """
        if torch.is_tensor(idx):
            idx = idx.tolist()
            # TODO: make work with torch.utils.data.Dataset
