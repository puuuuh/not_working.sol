use std::{process::Stdio, sync::atomic::AtomicBool};
mod output;

use camino::Utf8PathBuf;
pub use output::{Diagnostic, Output};
use tokio::io::AsyncReadExt;
use tokio::{
    select,
    sync::{mpsc, oneshot},
};
use tracing::warn;

#[derive(Clone)]
pub struct Flycheck {
    cmd: tokio::sync::mpsc::Sender<oneshot::Sender<Output>>,
}

impl Flycheck {
    pub fn new(root: Utf8PathBuf) -> Self {
        let (tx, rx) = mpsc::channel(1);
        tokio::runtime::Handle::current().spawn(Self::background_worker(root, rx));
        Self { cmd: tx }
    }

    async fn background_worker(
        root: Utf8PathBuf,
        mut channel: mpsc::Receiver<oneshot::Sender<Output>>,
    ) {
        let mut opt_task = None;
        let mut output = Vec::new();
        loop {
            // TODO: Fix this?
            if opt_task.is_none() {
                opt_task = channel.recv().await;
            }
            while !channel.is_empty() {
                opt_task = channel.recv().await;
            }
            if channel.is_closed() {
                opt_task = None;
            }
            let Some(task) = opt_task.take() else {
                return;
            };
            let mut process = match tokio::process::Command::new("forge")
                .args(["compile", "--json"])
                .current_dir(&root)
                .stdout(Stdio::piped())
                .spawn()
            {
                Ok(process) => process,
                Err(e) => {
                    warn!("Can't spawn forge for flycheck: {e}");
                    continue;
                }
            };

            let mut o = process.stdout.take().unwrap();
            output.clear();

            select! {
                _ = o.read_to_end(&mut output) => {},
                next_task = channel.recv() => {
                    _ = process.kill().await;
                    opt_task = next_task;
                    continue;
                }
            };

            let res = match serde_json::from_slice::<Output>(&output) {
                Ok(output) => output,
                Err(e) => {
                    warn!("Forge output deserialization error: {e}");
                    continue;
                }
            };

            _ = task.send(res);
        }
    }

    pub async fn check(&self) -> tokio::sync::oneshot::Receiver<Output> {
        let (tx, rx) = oneshot::channel();
        self.cmd.send(tx).await.expect("background worker is dead");

        rx
    }
}
