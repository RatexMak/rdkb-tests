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
package com.automatics.rdkb.tests.webpa;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWebPAServiceConfigurations extends AutomaticsTestBase{
    /**
     * Verify service configuration feature is disabled
     * <ol>
     * <li>Verify CcspServiceManager process is not running.</li>
     * <li>Verify CcspServiceManager binary is not present under /usr/bin directory.</li>
     * <li>Verify ccsp-servicemanager-broadband directory is NOT present under /usr/ccsp.</li>
     * <li>Verify ServiceManagerlog.txt.0 is not present in /rdklogs/logs</li>
     * <li>Verify "dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services." is not present</li>
     * <li>Verify /usr/lib/libsvc_agt.so library is not present</li>
     * </ol>
     * 
     * @author Anuvarshini M A
     * @refactor Govardhan
     * @param device
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-CCSP-1000")
    public void testVerifyCcspServiceManagerDisabled(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-CCSP-100";
	String stepNum = "";
	String errorMessage = "";
	String response = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CCSP-1000");
	LOGGER.info("TEST DESCRIPTION: Verify service configuration feature is disabled");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify CcspServiceManager process is not running.");
	LOGGER.info("2. Verify CcspServiceManager binary is not  present under /usr/bin directory. ");
	LOGGER.info("3. Verify ccsp-servicemanager-broadband directory is NOT present under /usr/ccsp.");
	LOGGER.info("4. Verify ServiceManagerlog.txt.0 is not present in  /rdklogs/logs");
	LOGGER.info("5. Verify \"dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services.\"  is not present ");
	LOGGER.info("6. Verify /usr/lib/libsvc_agt.so library is not present both in atom and arm console");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    errorMessage = "CcspServiceManager process is running";
	    boolean isAtomSyncAvailable = false;
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify CcspServiceManager process is not running.");
	    LOGGER.info("STEP 1: ACTION : Execute command pidof CcspServiceManager");
	    LOGGER.info("STEP 1: EXPECTED : CcspServiceManager process should not be running in console");
	    LOGGER.info("**********************************************************************************");

	    String ccspservicemanagerPid = null;
	    isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    ccspservicemanagerPid = isAtomSyncAvailable
		    ? BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
			    BroadBandCommandConstants.PROCESS_NAME_CCSPSERVICEMANAGER)
		    : CommonMethods.getPidOfProcess(device, tapEnv,
			    BroadBandCommandConstants.PROCESS_NAME_CCSPSERVICEMANAGER);
	    status = CommonMethods.isNull(ccspservicemanagerPid);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : CcspServiceManager process is not running");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    errorMessage = "CcspServiceManager binary is present";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify CcspServiceManager binary is not  present under /usr/bin directory. ");
	    LOGGER.info("STEP 2: ACTION : Execute command:ls -lrt /usr/bin/CcspServiceManager");
	    LOGGER.info(
		    "STEP 2: EXPECTED : CcspServiceManager binary should not be present.Response should be ls: /usr/bin/CcspServiceManager: No such file or directory");
	    LOGGER.info("**********************************************************************************");

	    status = isAtomSyncAvailable
		    ? !BroadBandSystemUtils.verifyFileAvailabilityInAtomConsole(tapEnv, device,
			    BroadBandCommandConstants.PROCESS_NAME_CCSPSERVICEMANAGER)
		    : !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandCommandConstants.PROCESS_NAME_CCSPSERVICEMANAGER);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : CcspServiceManager binary is not  present under /usr/bin directory");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S3";
	    errorMessage = "ccsp-servicemanager-broadband directory  is present under /usr/ccsp.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify ccsp-servicemanager-broadband directory is NOT present under /usr/ccsp.");
	    LOGGER.info("STEP 3: ACTION : Execute command:ls /usr/ccsp ");
	    LOGGER.info(
		    "STEP 3: EXPECTED : ccsp-servicemanager-broadband directory should not be present under /usr/ccsp.");
	    LOGGER.info("**********************************************************************************");

	    status = isAtomSyncAvailable
		    ? !BroadBandSystemUtils.verifyFileAvailabilityInAtomConsole(tapEnv, device,
			    BroadBandCommandConstants.LIB_CCSPSERVICEMANAGERBROADBAND)
		    : !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandCommandConstants.LIB_CCSPSERVICEMANAGERBROADBAND);

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : ccsp-servicemanager-broadband directory should not be present under /usr/ccsp");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S4";
	    errorMessage = "ServiceManagerlog.txt.0 is present in  /rdklogs/logs";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify ServiceManagerlog.txt.0 is not present in  /rdklogs/logs");
	    LOGGER.info("STEP 4: ACTION : Execute command:ls /rdklogs/logs");
	    LOGGER.info("STEP 4: EXPECTED : ServiceManagerlog.txt.0 should not be present in  /rdklogs/logs");
	    LOGGER.info("**********************************************************************************");

	    status = isAtomSyncAvailable
		    ? !BroadBandSystemUtils.verifyFileAvailabilityInAtomConsole(tapEnv, device,
			    BroadBandCommandConstants.LOG_SERVICEMANAGERLOG)
		    : !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandCommandConstants.LOG_SERVICEMANAGERLOG);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : ServiceManagerlog.txt.0 should not be present in  /rdklogs/logs");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S5";
	    errorMessage = "Parameter dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services. is present";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify \"dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services.\"is not present ");
	    LOGGER.info(
		    "STEP 5: ACTION :  Execute command:\"dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services.\" ");
	    LOGGER.info(
		    "STEP 5: EXPECTED : The parameter \"dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services.\" should not be present.Response should be \"Can\"t find destination component.\"");
	    LOGGER.info("**********************************************************************************");
	    String WebpaOutput = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_RBUS_ENABLE);
	    String expectedOutput = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.TRUE, WebpaOutput)
			    ? BroadBandTestConstants.PATTERN_DMCLIPARAMETER_NOT_FOUND_DBUS_MODE
			    : BroadBandTestConstants.PATTERN_DMCLIPARAMETER_NOT_FOUND;
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.CMD_DMCLI_GET_VALUE,
			    AutomaticsConstants.SPACE, BroadBandWebPaConstants.WEBPA_PARAM_SERVICEAGENT));
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, expectedOutput);
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : The parameter \"dmcli eRT getv Device.X_RDKCENTRAL_COM_ServiceAgent.Services.\" is not available");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S6";
	    errorMessage = "The Service Agent library is present";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify /usr/lib/svc_agt.so library is not present");
	    LOGGER.info("STEP 6: ACTION : Execute command:ls -lrt /usr/lib/*svc_agt*");
	    LOGGER.info(
		    "STEP 6: EXPECTED : The library should not be displayed both in arm and atom console.Response should be ls: /usr/lib/*svc_agt*: No such file or directory");
	    LOGGER.info("**********************************************************************************");

	    status = isAtomSyncAvailable
		    ? !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandCommandConstants.LIB_SVCAGENT)
			    && !BroadBandSystemUtils.verifyFileAvailabilityInAtomConsole(tapEnv, device,
				    BroadBandCommandConstants.LIB_SVCAGENT)
		    : !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandCommandConstants.LIB_SVCAGENT);

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : The Service agent library is not displayed .Response should be ls: /usr/lib/*svc_agt*: No such file or directory");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-CCSP-1000");
    }
}
