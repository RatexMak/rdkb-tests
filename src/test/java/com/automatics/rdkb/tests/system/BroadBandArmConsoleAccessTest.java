/**
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.system;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandArmConsoleAccessTest extends AutomaticsTestBase {

	/**
	 * verify the installed busy box utilities on CM side
	 * 
	 * <li>1. Verify binary module du using BusyBox and du was executable</li>
	 * <li>2. Verify binary module df using BusyBox and df was executable</li>
	 * <li>3. Verify binary module top using BusyBox and top was executable</li>
	 * <li>4. Verify binary module uptime using BusyBox and uptime was
	 * executable</li>
	 * <li>5. Verify binary module touch using BusyBox and touch was executable</li>
	 * <li>6. Verify binary module dmesg using BusyBox and dmesg was executable</li>
	 * 
	 * @param device {@link Dut}
	 * @author Leela Krishnama Naidu Andela
	 * @refactor Alan_Bivera
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-BUSYBOX_UTILITY-1003")
	public void testToVerifyBusyBoxUtilities(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE:TC-RDKB-BUSYBOX_UTILITY-1003");
		LOGGER.info("TEST DESCRIPTION: Test to verify the installed busy box utilities on CM side");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify binary module du using BusyBox and du was executable");
		LOGGER.info("2. Verify binary module df using BusyBox and df was executable");
		LOGGER.info("3. Verify binary module top using BusyBox and top was executable");
		LOGGER.info("4. Verify binary module uptime using BusyBox and uptime was executable");
		LOGGER.info("5. Verify binary module touch using BusyBox and touch was executable");
		LOGGER.info("6. Verify binary module dmesg using BusyBox and dmesg executable");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-BUSYBOX_UTILITY-103";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response from util execution
		String utilExecute = null;
		// String to store response
		String response = null;
		// String to store command
		String command = null;
		// Integer to store step count
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		// variable declaration ends

		for (String modules : BroadBandCommandConstants.EXECUTABLE_BUSYBOX_UTILS) {
			try {
				stepNumber = "s" + stepCount;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify binary executable module " + modules
						+ " using BusyBox");
				LOGGER.info("STEP " + stepCount + ": ACTION: 1. Execute command on Arm side: ls -l " + modules
						+ "using BusyBox" + "2. Execute command : sudo -S stbsshv6 ECM IP" + modules + " on CM side");
				LOGGER.info("STEP " + stepCount + ": EXPECTED: " + modules
						+ " binary module should use the BusyBox & module should be executable");
				LOGGER.info("******************************************************************************");
				errorMessage = "Binary module " + modules + " not using busybox";
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS_L,
						modules);
				response = tapEnv.executeCommand(device, command);
				if (CommonMethods.isNotNull(response)) {
					status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
							BroadBandCommandConstants.PROCESS_BUSY_BOX);
				}

				if (status) {
					errorMessage = "Binary module " + modules + " not executable on CM side";
					utilExecute = tapEnv.executeCommand(device, modules);
					status = CommonMethods.isNotNull(utilExecute) && !CommonUtils
							.isGivenStringAvailableInCommandOutput(utilExecute, BroadBandTestConstants.CMD_NOT_FOUND);
				}
				if (status) {
					LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified " + modules
							+ " using busybox & executable");
				} else {
					LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
				// ##################################################################################################//
				stepCount = stepCount + 1;

			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error("Exception Occurred while Verifying binary executables are using BusyBox" + errorMessage);
				CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status,
						errorMessage, false);
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-BUSYBOX_UTILITY-1003");
		// ###############################################################//
	}

	/**
	 * Validate Busybox utility tar
	 * <ol>
	 * <li>Validate tar is installed</li>
	 * <li>Tar rdklog files into home directory</li>
	 * <li>Create directory in home to store untar files</li>
	 * <li>Untar tar files to a directory</li>
	 * <li>Validate whether directory contains log files</li>
	 * <li>Verify the chain.cert.pem on the Gateway</li>
	 * <li>Verify the radiussrv.cert.pem on the Gateway</li>
	 * </ol>
	 * 
	 * @param device
	 * @author prasanthreddy.a,Muthukumar
	 * @refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-BUSYBOX-UTILITY-1001")
	public void testToVerifyTarInArmConsole(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-BUSYBOX-UTILITY-101";
		String stepNum = "S1";
		String errorMessage = "";
		boolean status = false;
		String response = null;
		String tmpPath = CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.SLASH_SYMBOL,
				BroadBandTestConstants.FOLDER_TMP, BroadBandTestConstants.SLASH_SYMBOL);
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-BUSYBOX-UTILITY-1001");
		LOGGER.info("TEST DESCRIPTION: Validate Busybox utility tar ");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Validate tar is installed ");
		LOGGER.info("2. Tar rdklog files into home directory");
		LOGGER.info("3. Create directory in home to store untar files");
		LOGGER.info("4. Untar tar files to a directory");
		LOGGER.info("5. Validate whether directory contains log files");
		LOGGER.info("6. Verify the chain.cert.pem on the Gateway");
		LOGGER.info("7. Verify the radiussrv.cert.pem on the Gateway");
		LOGGER.info("#######################################################################################");

		try {

			stepNum = "S1";
			errorMessage = "Unable to validate response";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Validate tar is installed ");
			LOGGER.info("STEP 1: ACTION : Execute :which tar");
			LOGGER.info("STEP 1: EXPECTED : Successfully validate installed status");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TAR_CHECK);

			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.PATH_TO_TAR);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully verified tar availability in console");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S2";
			errorMessage = "Unable to tar log files";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Tar rdklog files into home directory");
			LOGGER.info("STEP 2: ACTION : Execute :tar -zcvf /home/root/sample.tar.gz /rdklogs/logs/");
			LOGGER.info("STEP 2: EXPECTED : Successfully grouped files to tar");
			LOGGER.info("**********************************************************************************");

			String tarResponse = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_TAR_RDKLOG_FILE
					.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.TAR_TEMPORARY_FILE));

			status = CommonMethods.isNotNull(tarResponse)
					&& !CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.TEXT_RO_FILE_SYSTEM);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully grouped log files to tar ");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S3";
			errorMessage = "Unable to create directory";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Create directory in home to store untar files");
			LOGGER.info("STEP 3: ACTION : Execute : mkdir test");
			LOGGER.info("STEP 3: EXPECTED : Successfully created directory");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device,
					CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_MKDIR,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, tmpPath,
							BroadBandTestConstants.TEMPORARY_FOLDER));

			response = tapEnv.executeCommandUsingSsh(device, CommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.CMD_LS, BroadBandTestConstants.SINGLE_SPACE_CHARACTER, tmpPath));

			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.TEMPORARY_FOLDER);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully created directory to save untar files");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S4";
			errorMessage = "Unable to untar log files";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Untar tar files to a directory");
			LOGGER.info("STEP 4: ACTION : Execute : tar -C /home/root/test -xvf /home/root/sample.tar.gz");
			LOGGER.info("STEP 4: EXPECTED : Successfully untarred files to directory");
			LOGGER.info("**********************************************************************************");
			try {
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_TO_UNTAR_GIVEN_FILE
								.replace(BroadBandTestConstants.STRING_REPLACE,
										CommonUtils.concatStringUsingStringBuffer(tmpPath,
												BroadBandTestConstants.TEMPORARY_FOLDER))
								.replace(BroadBandTestConstants.STRING_VALUE_TO_REPLACE,
										CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.NVRAM_PATH,
												BroadBandTestConstants.TAR_TEMPORARY_FILE)));

				status = CommonMethods.isNotNull(response)
						&& CommonUtils.patternSearchFromTargetString(tarResponse, response);

				if (!status) {
					status = CommonUtils.rebootUsingWebpaAndWaitForIpAcquisition(tapEnv, device);

					response = tapEnv.executeCommandUsingSsh(device,
							BroadBandCommandConstants.CMD_TO_UNTAR_GIVEN_FILE
									.replace(BroadBandTestConstants.STRING_REPLACE,
											CommonUtils.concatStringUsingStringBuffer(tmpPath,
													BroadBandTestConstants.TEMPORARY_FOLDER))
									.replace(BroadBandTestConstants.STRING_VALUE_TO_REPLACE,
											CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.NVRAM_PATH,
													BroadBandTestConstants.TAR_TEMPORARY_FILE)));

					status = CommonMethods.isNotNull(response)
							&& CommonUtils.patternSearchFromTargetString(tarResponse, response);

				}
			} catch (Exception ex) {
				LOGGER.error("Exception occurred while untar files " + ex.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully untar files to temporary directory");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S5";
			errorMessage = "Unable to validate response";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Validate whether directory contains log files");
			LOGGER.info("STEP 5: ACTION : Execute: ls /home/root/test/rdklogs/logs/");
			LOGGER.info("STEP 5: EXPECTED : Successfully validated response");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device,
					CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, tmpPath,
							BroadBandTestConstants.TEMPORARY_FOLDER, BroadBandCommandConstants.DIRECTORY_LOGS));

			status = CommonMethods.isNotNull(response) && !CommonUtils.patternSearchFromTargetString(response,
					BroadBandTraceConstants.LOG_MESSAGE_GREP_NO_SUCH_FILE_OR_DIRECTORY);

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Successfully validated folder ");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			if (!(DeviceModeHandler.isDSLDevice(device) && DeviceModeHandler.isFibreDevice(device)
					&& CommonMethods.isAtomSyncAvailable(device, tapEnv))) {
				/**
				 * STEP : VERIFY THE CHAIN.CERT.PEM ON THE GATEWAY
				 */
				stepNum = "S6";
				status = false;
				errorMessage = "Failed to verify the chain.cert.pem on the Gateway";
				if (!DeviceModeHandler.isRPIDevice(device)) {
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 6 : DESCRIPTION : Verify the chain.cert.pem on the Gateway");
					LOGGER.info(
							"STEP 6 : ACTION : Execute command:/usr/bin/openssl x509 -enddate -noout -in /tmp/lnf/certs/ca-chain.cert.pem");
					LOGGER.info("STEP 6 : EXPECTED : Must return the chain.cert.pem");
					LOGGER.info("**********************************************************************************");
					try {
						response = tapEnv.executeCommandUsingSsh(device,
								BroadBandCommandConstants.CMD_TO_GET_CA_CHAIN_CERT_PEM);
						status = CommonMethods.isNotNull(response)
								&& CommonMethods.patternMatcher(response, AutomaticsTapApi
										.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_GET_CA_CHAIN_CERT_PEM));
					} catch (Exception e) {
						LOGGER.error(errorMessage += e.getMessage());
					}
					if (status) {
						LOGGER.info("STEP 6 : ACTUAL : Successfully verified the chain.cert.pem on the Gateway");
					} else {
						LOGGER.error("STEP 6 : ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				} else {
					LOGGER.info("not applicable for RPi... skipping teststep ...");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
				}

				/**
				 * STEP : VERIFY THE RADIUSSRV.CERT.PEM ON THE GATEWAY
				 */
				stepNum = "S7";
				status = false;
				errorMessage = "Failed to verify the radiussrv.cert.pem on the Gateway";
				if (!DeviceModeHandler.isRPIDevice(device)) {
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 7 : DESCRIPTION : Verify the radiussrv.cert.pem on the Gateway");
					LOGGER.info(
							"STEP 7 : ACTION : Execute command:/usr/bin/openssl x509 -enddate -noout -in /tmp/lnf/certs/radiussrv.cert.pem");
					LOGGER.info("STEP 7 : EXPECTED : Must return the chain.cert.pem");
					LOGGER.info("**********************************************************************************");
					try {
						response = tapEnv.executeCommandUsingSsh(device,
								BroadBandCommandConstants.CMD_TO_GET_RADIUSSRV_CERT_PEM);
						status = CommonMethods.isNotNull(response)
								&& CommonMethods.patternMatcher(response, AutomaticsTapApi
										.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_GET_RADIUSSRV_CERT_PEM));
					} catch (Exception e) {
						LOGGER.error(errorMessage += e.getMessage());
					}
					if (status) {
						LOGGER.info("STEP 7 : ACTUAL : Successfully verified the radiussrv.cert.pem on the Gateway");
					} else {
						LOGGER.error("STEP 7 : ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				} else {
					LOGGER.info("not applicable for RPi... skipping teststep ...");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
				}
			} else {
				int stepNumber = 6;
				while (stepNumber <= 7) {
					stepNum = "S" + stepNumber;
					errorMessage = "STEP " + stepNumber + ": ACTUAL : NOT APPLICABLE FOR DEVICE MODEL :"
							+ device.getModel();
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNumber++;
				}
			}
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Delete temporary directory and tar file");
			LOGGER.info("POST-CONDITION : ACTION : Execute rm -r /home/root/test and rm /home/root/sample.tar.gz");
			LOGGER.info("POST-CONDITION : EXPECTED : Successfully deleted temporary files ");
			LOGGER.info("#######################################################################################");
			try {
				response = tapEnv.executeCommandUsingSsh(device,
						CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TO_REMOVE,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER, tmpPath,
								BroadBandTestConstants.TEMPORARY_FOLDER, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.SEMI_COLON, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandCommandConstants.CMD_TO_REMOVE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.NVRAM_PATH, BroadBandTestConstants.TAR_TEMPORARY_FILE));
				status = !CommonUtils.patternSearchFromTargetString(response,
						BroadBandTraceConstants.LOG_MESSAGE_GREP_NO_SUCH_FILE_OR_DIRECTORY);

			} catch (Exception e) {
				LOGGER.error("Exception occurred while removing temporary files " + e.getMessage());
			}
			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL :Successfully removed temporary files and folders");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Failed to remove temporary files");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-BUSYBOX-UTILITY-1001");
	}

}
