/*
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
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DataMountUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class DataMountExecutableTest extends AutomaticsTestBase {
    /**
     *
     * Test Case # 1: Verify the data only partition /nvram is mounted as non-executable.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify executing the shell script on data only mount partition /nvram.</li>
     * <li>S2) Verify executing the shell script on data only mount partition /nvram, after setting executable
     * permission for all users.</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Govardhan
     * @param device
     *            {@link Dut}
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SYSTEM })

    @TestDetails(testUID = "TC-RDKB-SYSTEM-5502")
    public void testNvramMount(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-552";
	boolean result = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    result = CommonUtils.doesDirectoryExist(device, tapEnv, BroadBandCommandConstants.MOUNT_NVRAM);
	    LOGGER.info("### PRE-CONDITION ### DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM + " EXISTS : "
		    + result);
	    if (!result) {
		errorMessage = "DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM
			+ " DOES NOT EXIST. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    String firmwareVersion = ((Device) device).getFirmwareVersion();
	    LOGGER.info("DEVICE FIRMWARE VERSION: " + firmwareVersion);
	    boolean isProdBuild = CommonMethods.isNotNull(firmwareVersion)
		    && BroadbandPropertyFileHandler.verifyIsProdBuildOnDevice(firmwareVersion);
	    LOGGER.info("IS PROD BUILD: " + isProdBuild);

	    /**
	     * S1) Verify executing the shell script on data only mount partition /nvram.
	     */
	    LOGGER.info("STEP 1: DESCRIPTION : Verify executing the shell script on data only mount partition /nvram");
	    LOGGER.info(
		    "STEP 1: ACTION : Create a shell script using command: echo -e \"#!/bin/bash\\n echo \"\"Hello World\"\" > /nvram/rdkb10618.sh and Execute the script as: /nvram/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 1: EXPECTED: Shell Script must be created successfully and when executed It must throw 'Permission denied'");
	    result = DataMountUtils.createShellScript(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM);
	    LOGGER.info("SHELL SCRIPT CREATED FOR EXECUTION: " + result);
	    if (!result) {
		errorMessage = "TEST SHELL SCRIPT COULD NOT BE CREATED. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    result = DataMountUtils.verifyScriptExecution(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM,
		    isProdBuild);
	    errorMessage = isProdBuild
		    ? "Shell Script got Created successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and did not thrown Permission denied error.";
	    String successMessage = isProdBuild
		    ? "Shell Script got Created successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S2) Verify executing the shell on data only mount partition /nvram, after setting executable permission
	     * for all users.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify executing shell script with permission as '777' on /nvram mount.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute command: chmod 777 /nvram/rdkb10618.sh and Execute the script as: /nvram/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 2: EXPECTED: Shell Script must be executed successfully, In case of prod build it should throw Permission denied.");
	    result = DataMountUtils.verifyScriptExecutionAfterGrantingPermission(tapEnv, device,
		    BroadBandCommandConstants.MOUNT_NVRAM, isProdBuild);

	    errorMessage = isProdBuild
		    ? "Shell Script got executed successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script did not get executed successfully and thrown Permission denied error.";
	    successMessage = isProdBuild
		    ? "Shell Script did not get executed successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got executed successfully and did not thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 2: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VALIDATING THE DATA ONLY STORAGE PARTITION /nvram IS MOUNTED AS NON-EXECUTABLE: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("### POST-CONDITION ### REMOVE THE TEST SHELL SCRIPT: " + BroadBandCommandConstants.MOUNT_NVRAM
		    + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    result = CommonUtils.deleteFile(device, tapEnv,
		    BroadBandCommandConstants.MOUNT_NVRAM + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    LOGGER.info("### POST-CONDITION ### REMOVED THE TEST SHELL SCRIPT: " + result);
	}
    }

    /**
     *
     * Test Case # 2: Verify the data only partition /nvram2 is mounted as non-executable.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify executing the shell script on data only mount partition /nvram2.</li>
     * <li>S2) Verify executing the shell script on data only mount partition /nvram2, after setting executable
     * permission for all users.</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SYSTEM })

    @TestDetails(testUID = "TC-RDKB-SYSTEM-5503")
    public void testNvram2Mount(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-553";
	boolean result = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    result = CommonUtils.doesDirectoryExist(device, tapEnv, BroadBandCommandConstants.MOUNT_NVRAM2);
	    LOGGER.info("### PRE-CONDITION ### DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM2 + " EXISTS : "
		    + result);
	    if (!result) {
		errorMessage = "DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM2
			+ " DOES NOT EXIST. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    String firmwareVersion = ((Device) device).getFirmwareVersion();
	    LOGGER.info("DEVICE FIRMWARE VERSION: " + firmwareVersion);
	    boolean isProdBuild = CommonMethods.isNotNull(firmwareVersion)
		    && BroadbandPropertyFileHandler.verifyIsProdBuildOnDevice(firmwareVersion);
	    LOGGER.info("IS PROD BUILD: " + isProdBuild);

	    /**
	     * S1) Verify executing the shell script on data only mount partition /nvram2.
	     */
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify executing the shell script on data only mount partition /nvram2.");
	    LOGGER.info(
		    "STEP 1: ACTION : Create a shell script using command: echo -e \"#!/bin/bash\\n echo \"Hello World\"\" > /nvram2/rdkb10618.sh and Execute the script as: /nvram2/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 1: EXPECTED: Shell Script must be created successfully and when executed it must throw 'Permission denied'");
	    result = DataMountUtils.createShellScript(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM2);
	    if (!result) {
		errorMessage = "TEST SHELL SCRIPT COULD NOT BE CREATED. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    result = DataMountUtils.verifyScriptExecution(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM2,
		    isProdBuild);
	    errorMessage = isProdBuild
		    ? "Shell Script got Created successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and did not thrown Permission denied error.";
	    String successMessage = isProdBuild
		    ? "Shell Script got Created successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S2) Verify executing the shell script on data only mount partition /nvram2, after setting executable
	     * permission for all users.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify executing shell scrip with permission as '777' on /nvram2 mount.");
	    LOGGER.info("STEP 2: ACTION : Execute command: chmod 777 /nvram2/rdkb10618.sh and Execute the script as: nvram2/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Shell Script must be executed successfully, In case of prod build it should throw Permission denied.");
	    result = DataMountUtils.verifyScriptExecutionAfterGrantingPermission(tapEnv, device,
		    BroadBandCommandConstants.MOUNT_NVRAM2, isProdBuild);
	    errorMessage = isProdBuild
		    ? "Shell Script got executed successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script did not get executed successfully and thrown Permission denied error.";
	    successMessage = isProdBuild
		    ? "Shell Script did not get executed successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got executed successfully and did not thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 2: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VALIDATING THE DATA ONLY STORAGE /nvram2 PARTITION IS MOUNTED AS NON-EXECUTABLE: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("### POST-CONDITION ### REMOVE THE TEST SHELL SCRIPT: " + BroadBandCommandConstants.MOUNT_NVRAM2
		    + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    result = CommonUtils.deleteFile(device, tapEnv,
		    BroadBandCommandConstants.MOUNT_NVRAM2 + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    LOGGER.info("### POST-CONDITION ### REMOVED THE TEST SHELL SCRIPT: " + result);
	}
    }

    /**
     * Verify Mount options for tmp directory
     * 
     * <li>1. Validate tmp directory has mount options of nodev, nosuid, noexec</li>
     * <li>2. Create Shell script on tmp to validate no exec mount option on Arm and Atom</li>
     * <li>3. Validate tmp mount option noexec on Arm and Atom</li>
     * <li>4. Validate tmp mount option noexec with chmod 777 on Arm and Atom</li>
     * 
     * @author RamaTeja Meduri
	 * @refactor Athira
     */
    
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SYSTEM })

    @TestDetails(testUID = "TC-RDKB-SYSTEM-5400")
    public void testTmpMount(Dut device) {
	
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5400");
	LOGGER.info("TEST DESCRIPTION: Test to Verify Mount options for tmp directory.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Validate tmp directory has mount options of nodev, nosuid, noexec");
	LOGGER.info("2. Create Shell script on tmp to validate no exec mount option on Arm and Atom");
	LOGGER.info("3. Validate tmp mount option noexec on Arm and Atom");
	LOGGER.info("4. Validate tmp mount option noexec with chmod 777 on Arm and Atom");
	LOGGER.info("#######################################################################################");
	
	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-SYSTEM-540";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// Variable to store atom sync available
	boolean isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	// String to store response
	String response = null;
	// variable declaration ends

	try {
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Verify tmp directory is available on the device");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Execute linux Command:if [ -d /tmp/ ] ; then echo \"true\" ; else echo \"false\" ; fi");
	    LOGGER.info("PRE-CONDITION : EXPECTED : tmp directory should be present");
	    errorMessage = "tmp directory is not present";
	    status = CommonUtils.doesDirectoryExist(device, tapEnv, BroadBandCommandConstants.MOUNT_TMP);
	    if (status && isAtomSyncAvailable) {
		status = BroadBandCommonUtils.doesDirectoryExistInArmElseAtomConsole(device, tapEnv,
			BroadBandCommandConstants.MOUNT_TMP, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.TEN_SECOND_IN_MILLIS, BroadBandTestConstants.TRUE).isStatus();
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION : ACTUAL : tmp directory is presnt in the device");
	    } else {
		LOGGER.error("PRE-CONDITION : ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    stepNumber = "s1";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Validate tmp directory has mount options of nodev, nosuid, noexec");
	    LOGGER.info("STEP 1: ACTION: Execute linux Command:mount |grep -i \"/tmp type tmpfs\"");
	    LOGGER.info("STEP 1: EXPECTED: tmp should have mount options of nodev, nosuid, noexec");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "tmp is not having mount options of nodev, nosuid, noexec";
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.MOUNT_OPTIONS_TMP);
	    status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.MOUNT_OPTION_NODEV)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.MOUNT_OPTION_NOSUID)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.MOUNT_OPTION_NOEXEC);
	    if (status && isAtomSyncAvailable) {
		response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.MOUNT_OPTIONS_TMP);
		status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
			BroadBandTestConstants.MOUNT_OPTION_NODEV)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandTestConstants.MOUNT_OPTION_NOSUID)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandTestConstants.MOUNT_OPTION_NOEXEC);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: tmp has mount options of nodev, nosuid, noexec");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Create Shell script on tmp to validate no exec mount option");
	    LOGGER.info(
		    "STEP 2: ACTION: Execute linux Command:echo -e '#!/bin/bash\n echo \"HELLO WORLD\"' > rdkb10618.sh");
	    LOGGER.info("STEP 2: EXPECTED: Shell script must be successfully created");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Shell script  is not  created";
	    status = DataMountUtils.createShellScript(tapEnv, device, BroadBandCommandConstants.MOUNT_TMP);
	    if (status && isAtomSyncAvailable) {
		status = DataMountUtils.createShellScriptOnAtom(tapEnv, device, BroadBandCommandConstants.MOUNT_TMP);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL: Shell script is successfully created");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Validate tmp mount option noexec");
	    LOGGER.info("STEP 3: ACTION: Execute linux Command:/tmp/rdkb10618.sh");
	    LOGGER.info("STEP 3: EXPECTED: Script should not be executed and should display Permission denied ");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Permission denied is not displayed";
	    status = DataMountUtils.verifyScriptExecution(tapEnv, device, BroadBandCommandConstants.MOUNT_TMP,
		    BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
	    if (status && isAtomSyncAvailable) {
		status = DataMountUtils.verifyScriptExecutionOnAtom(tapEnv, device,
			BroadBandCommandConstants.MOUNT_TMP);
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Script is not executed and should display Permission denied");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s4";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: Validate tmp mount option noexec on 777 permission");
	    LOGGER.info("STEP 4: ACTION: Execute linux Command:chmod 777 /tmp/rdkb10618.sh and /tmp/rdkb10618.sh");
	    LOGGER.info("STEP 4: EXPECTED: Script should not be executed and should display Permission denied");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Permission denied is not displayed";
	    status = DataMountUtils.verifyScriptExecutionAfterGrantingPermissiononNonProdBuild(tapEnv, device,
		    BroadBandCommandConstants.MOUNT_TMP);
	    if (status && isAtomSyncAvailable) {
		status = DataMountUtils.verifyScriptExecutionAfterGrantingPermissionOnAtom(tapEnv, device,
			BroadBandCommandConstants.MOUNT_TMP);
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL: Script is not executed and should display Permission denied");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING MOUNT ON TMP FOLDER " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("### POST-CONDITION ### REMOVE THE TEST SHELL SCRIPT: " + BroadBandCommandConstants.MOUNT_TMP
		    + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    status = CommonUtils.deleteFile(device, tapEnv,
		    BroadBandCommandConstants.MOUNT_TMP + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    if (status && isAtomSyncAvailable) {
		status = BroadBandCommonUtils.deleteFileAndVerifyIfAtomPresentElseArm(tapEnv, device,
			BroadBandCommandConstants.MOUNT_TMP + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT,
			isAtomSyncAvailable);
	    }
	    LOGGER.info("### POST-CONDITION ### REMOVED THE TEST SHELL SCRIPT: " + status);
	}
	LOGGER.info("ENDING TEST CASE : TC-RDKB-SYSTEM-5400");
    }
}
