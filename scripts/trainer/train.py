import torch
import logging

from os.path import abspath, dirname, join
from torch.utils.data.dataloader import DataLoader as DataLoader

from sklearn.metrics import f1_score, confusion_matrix


logger = logging.getLogger("train.py")

logging.basicConfig(
    format="%(asctime)s [%(name)s: %(levelname)s] | %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.INFO,
)


class Trainer:
    def __init__(
        self,
        data_loader: DataLoader,
        test_data_loader: DataLoader,
        model: torch.nn.Module,
        optimizer,
        loss_fn,
        device: torch.device = None,
        output_dir=None,
        log_interval: int = 100,
        lr_scheduler=None,
    ):
        self._data_loader = data_loader
        self._test_data_loader = test_data_loader
        self._model = model.to(device)
        self._device = device
        self._optimizer = optimizer
        self._loss_fn = loss_fn
        self._lr_scheduler = lr_scheduler
        self._output_dir = output_dir
        self._log_interval = log_interval

        self._global_step = 0

    def _check_property(self, prop_name: str):
        """Check that a property has been set.

        attr_name: Name of attribute to check.
        """
        if not hasattr(self, prop_name):
            raise RuntimeError(f"{prop_name} needs to be set in {type(self).__name__}")

    @property
    def output_dir(self):
        self._check_property("_output_dir")
        return self._output_dir

    @property
    def global_step(self):
        return self._global_step

    def fit(
        self,
        epochs: int,
    ):
        """Train the model.

        args:
            epochs: Number of epochs per batch
        """
        self._global_step = 0
        total_steps = epochs * len(self._data_loader)
        self._model.train()
        # start_before()
        print("Starting Training")
        for epoch in range(epochs):
            runnning_loss = 0.0

            for batch_step, batch in enumerate(self._data_loader):

                # for mini_batch in zip(torch.split(batch[0], 2), torch.split(batch[1], 2)):
                for mini_batch in zip(batch[0], batch[1]):
                    # for X, y in zip(mini_batch[0], mini_batch[1]):
                    X, y = mini_batch
                    X = X.to(self._device)
                    y = y.type(torch.LongTensor).to(self._device)
                    # Zero the parameter gradients
                    self._optimizer.zero_grad()

                    # Forward + Backward + Optimize
                    outputs = self._model(X)
                    loss = self._loss_fn(outputs, y)
                    loss.backward()
                    self._optimizer.step()

                    curr_loss = loss.item()
                    if self._lr_scheduler is not None:
                        self._lr_scheduler.step()

                    runnning_loss += curr_loss
                    if self._global_step % 100 == 0:
                        print(f"Epoch: {self._global_step} | loss: {curr_loss}")

                    self._global_step += 1

    def save_model(self):
        """Save the feature vector."""
        if self._output_dir is not None:
            torch.save(
                self._model(),
                join(dirname(dirname(dirname(abspath(__file__)))), self._output_dir),
            )

    @torch.no_grad()    # We don't need to calculate gradient
    def eval(self):
        total_samples = 0
        correct = 0
        self._model.eval()
        y_true = []
        y_pred = []

        for global_step, batch in enumerate(self._test_data_loader):
            for mini_batch in zip(batch[0], batch[1]):
                X, y = mini_batch
                X, y = X.to(self._device), y.to(self._device)
                X = torch.as_tensor(X)
                y = torch.as_tensor(y)

                # calculate outputs by running images through the network
                outputs = self._model(X)

                _, predicted = torch.max(outputs.data, 1)
                total_samples += y.size(0)
                correct += (predicted == y).sum().item()
                y_true.append(y)
                y_pred.append(predicted)

        logging.info(
            "Accuracy of the network on the %d test images: %f",
            total_samples,
            (100 * correct // total_samples)
        )

        f1_score(y_true=y_true, y_pred=y_pred)

