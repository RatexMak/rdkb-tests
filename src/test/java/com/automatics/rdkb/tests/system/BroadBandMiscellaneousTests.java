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
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandMiscellaneousTests extends AutomaticsTestBase{
    
    /**
     * Test to verify configparamgen utility test
     * <p>
     * Steps:1
     * <ol>
     * <li>Copy configparamgen utility script file from VM to STB /tmp folder</li>
     * <li>configparamgen utility script file should be copied to STB /tmp folder</li>
     * </ol>
     * Steps:2
     * <ol>
     * <li>Run hardware selt test and verify test completed status</li>
     * <li>Script should be invoked and the script response should not be null
     * </ol>
     * Steps:3
     * <ol>
     * <li>Verify the success status for all the configparamgen tests</li>
     * <li>Invoked script response should return test status as SUCCESS
     * </ol>
     * </p>
     * 
     * @param device
     *            instance of {@link Dut}
     * 
     * @author Rahul Raveendran, TATA ELXSI
     * @refactor Said Hisham
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.NEW_FEATURE })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1000")
    public void testToverifyConfigParamGenUtility(Dut device) {

	/** command to execute config param gen utility script */
	final String CMD_EXECUTE_CONFIG_PARAM_GEN_UTILITY_SCRIPT = "sh /tmp/";

	// Test step validation status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-SYSTEM-000";
	// Test step number
	String testStepNumber = "s1";
	// Error message needs to show during test script failure
	String errorMessage = null;
	// String to store the command response
	String response = null;
	// getting the latest file path from stb.props
	String cpgFilePath = null;
	// getting the latest file name from stb.props
	String cpgFileName = null;

	LOGGER.info("STARTING TEST CASE:TC-RDKB-SYSTEM-1000");
	LOGGER.info("TEST DESCRIPTION: Test to Verify verify configparamgen utility test");
	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP 1: Copy configparamgen utility script file from AUtoVault to STB /tmp folder ");
	LOGGER.info("STEP 2: Invoke configparamgen utility script and verify the response ");
	LOGGER.info("STEP 3: Verify the success status for all the configparamgen tests ");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Download configparamgen utility script file from AutoValut to STB /tmp folder");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute curl -L < URL>/api/download?fileName=<FILE PATH> -o '<TARGET FILE PATH>' -w \\\"status_code:%{http_code}");
	    LOGGER.info("STEP 1: EXPECTED : configparamgen utility script file should be copied to STB /tmp folder");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Copy configparamgen utility script file from VM to STB /tmp folder ");
	    LOGGER.info("EXPECTED: configparamgen utility script file should be copied to STB /tmp folder ");

	    // getting the latest file path from stb.props
	    cpgFilePath = BroadbandPropertyFileHandler.getConfigparamgenPath();
	    // getting the latest file name from stb.props
	    cpgFileName = BroadbandPropertyFileHandler.getConfigparamgenFileName();
	    errorMessage = "failed to copy configparamgen utility script " + cpgFileName + " from VM to STB";
	    try {
		status = CommonUtils.downloadFileUsingAutoVault(device, tapEnv, cpgFilePath,
			BroadBandCommandConstants.FOLDER_PATH_TMP);
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    } catch (Exception e) {
		LOGGER.error(errorMessage = errorMessage + " Exception Occured " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Sucessfully downloaded configparamgen utility script file from AutoValut to STB /tmp folder");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /*
	     * s2. Verify the script execution is successful
	     */
	    testStepNumber = "s2";
	    status = false;
	    errorMessage = "Obtained null response..Not able to get the response for executing config param gen utility script";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Invoke configparamgen utility script and verify the response");
	    LOGGER.info("STEP 2: ACTION : Execute command sh /tmp/cpg-test.sh");
	    LOGGER.info("STEP 2: EXPECTED : Script should be invoked and the script response should not be null");
	    LOGGER.info("**********************************************************************************");
	    // execute config param utility script and verify the response
	    if (!DeviceModeHandler.isDSLDevice(device)) {
		String command = CMD_EXECUTE_CONFIG_PARAM_GEN_UTILITY_SCRIPT + cpgFileName;
		response = tapEnv.executeCommandUsingSsh(device, command);
		LOGGER.info("response For configparamgen========" + response);
	    }
	    status = (CommonMethods.isNotNull(response));
	    // verifying that the command is not returning null or no such file or directory
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Response is not null");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");

	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /*
	     * s3. Verify the success status for all the configparamgen tests
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify the success status for all the configparamgen tests ");
	    LOGGER.info("STEP 3: ACTION : Execute command sh /tmp/cpg-test.sh");
	    LOGGER.info("STEP 3: EXPECTED :Invoked script response should return test status as SUCCESS ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to get any response for configparamgen script test!!";
	    if (CommonMethods.isNotNull(response)) {
		if (response.contains(BroadBandTestConstants.STRING_SUCCESS_STATUS_FOR_CONFIGPARAM_TESTS)) {
		    status = true;
		} else {
		    String numberOfFailedTests = CommonMethods.patternFinder(response,
			    BroadBandTestConstants.PATTERN_GET_FAILURE_STEPS_FOR_CONFIGPARAM_TESTS);
		    errorMessage = "Configparamgen test run failed!! No: of Tests failed: " + numberOfFailedTests;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Invoked script response should return test status as SUCCESS");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = "Exception Occured because of ==>" + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} finally {
	    tapEnv.executeCommandUsingSsh(device, "rm -rf /tmp/" + cpgFileName);
	}

	LOGGER.info("ENDING TEST CASE:TC-RDKB-SYSTEM-1000");
    }
    
    /**
     * <ol>
     * <li>Verify whether webpa process is up</li>
     * <li>Create procanalyzerconfig.ini under nvram with dynamic values and verify file procanalyzerconfig.ini</li>
     * <li>Create processes.list under nvram anf verify /nvram/processes.list</li>
     * <li>Check the default value for the parameter "Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable" using webpa</li>
     * <li>Verify UPLOAD_LOGS_VAL_DCM value is set to true using sysevent</li>
     * <li>Enable the parameter "Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable" using webpa</li>
     * <li>Verify the process is up for /usr/bin/cpuprocanalyzer</li>
     * <li>Verify the default values under /etc/procanalyzerconfig.ini is "FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60
     * FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1"</li>
     * <li>Check the captured data is saved under /tmp/cpuprocanalyzer</li>
     * <li>Verify /tmp/cpuprocanalyzer folder must not exceed 1.5MB</li>
     * <li>Verify CPUPROCANALYZERlog.txt.0 log is created</li>
     * <li>Veirfy console log on the log upload for the CPAstats</li>
     * <li>Verify the "/usr/bin/cpuprocanalyzer" exited after the log run.</li>
     * <li>Verify captured data in cpuprocanalyzer is not persistent on post reboot.</li>
     * 
     * @author Anuvarshini Manickavasagam Arulnambi
     * @refactor Said Hisham
     *           </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-CPUPROC-1000")
    public void testToVerifyCpuprocanalyzer(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-CPUPROC-100";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CPUPROC-1000");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1.Verify whether webpa process is up");
	LOGGER.info(
		"2. Create procanalyzerconfig.ini under nvram with dynamic values  and verify file procanalyzerconfig.ini ");
	LOGGER.info("3. Create processes.list under nvram anf verify /nvram/processes.list");
	LOGGER.info(
		"4. Check the default value for the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	LOGGER.info("5. Verify UPLOAD_LOGS_VAL_DCM value is set to true using sysevent");
	LOGGER.info("6. Enable the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	LOGGER.info("7. Verify the process is up for /usr/bin/cpuprocanalyzer");
	LOGGER.info(
		"8. Verify the default values under /etc/procanalyzerconfig.ini is FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	LOGGER.info("9. Check the captured data is saved under /tmp/cpuprocanalyzer");
	LOGGER.info("10. Verify /tmp/cpuprocanalyzer folder must not exceed 1.5MB");
	LOGGER.info("11. Verify CPUPROCANALYZERlog.txt.0 log is created ");
	LOGGER.info("12. Verify console log on the log upload for the CPAstats");
	LOGGER.info("13. Verify the /usr/bin/cpuprocanalyzer exited after the log run.");
	LOGGER.info("14. Verify captured data in cpuprocanalyzer is not persistent on post reboot.");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Webpa process is not up";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify whether webpa process is up");
	    LOGGER.info("STEP 1: ACTION : Execute command:pidof webpa");
	    LOGGER.info("STEP 1: EXPECTED : Webpa process should be up");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : WebPA is Up and Running in the Device");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("***********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "procanalyzerconfig.ini is not created under nvram ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Create procanalyzerconfig.ini under nvram with dynamic values  and verify file procanalyzerconfig.ini ");
	    LOGGER.info(
		    "STEP 2: ACTION : Copy procanalyzerconfig.ini from autovault\"Add below values:FEATURE.CPUPROCANALYZER.SLEEP.SECS = 30FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 90FEATURE.CPUPROCANALYZER.DYNAMIC = 1and save.Check cat /nvram/procanalyzerconfig.ini for saved values");
	    LOGGER.info("STEP 2: EXPECTED : File procanalyzerconfig.ini should get created under nvram.");
	    LOGGER.info("**********************************************************************************");

	    CommonUtils.downloadFileUsingAutoVault(device, tapEnv,
		    BroadbandPropertyFileHandler.getCpuProcAnalyzerFile(), BroadBandCommandConstants.MOUNT_NVRAM);
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.FEATURE_CPUPROCANALYZER_DYNAMIC,
		    BroadBandCommandConstants.FILE_PATH_CPU_PROC_NVRAM));

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : procanalyzerconfig.ini is created under nvram");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Processes not saved under /nvram/processes.list";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Create processes.list under nvram anf verify /nvram/processes.list");
	    LOGGER.info(
		    "STEP 3: ACTION : Copy processes.list from autovault\"Check if processes got saved in /nvram/processes.list Execute command:cat /nvram/processes.list");
	    LOGGER.info("STEP 3: EXPECTED : Processes should get saved in /nvram/processes.list");
	    LOGGER.info("**********************************************************************************");

	    CommonUtils.downloadFileUsingAutoVault(device, tapEnv, BroadbandPropertyFileHandler.getProcessListFile(),
		    BroadBandCommandConstants.MOUNT_NVRAM);
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandCommandConstants.CCSP_COMPONENT_PROCESS_NAME,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PROCESSES));

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Processes are saved under /nvram/processes.list");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "The default value of parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\"is not false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Check the default value for the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command:curl \"https://<url:port>/api/v2/device/mac:<MAC>/config?names=Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" -H \"AuThorization: Bearer <SAT token>\"");
	    LOGGER.info(
		    "STEP 4: EXPECTED : The default value of parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\"should be false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_CPU_PROC_ANALYZER, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL :The default value of parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\"is retrieved as false");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "The value UPLOAD_LOGS_VAL_DCM is not set to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify UPLOAD_LOGS_VAL_DCM value is set to true using sysevent");
	    LOGGER.info("STEP 5: ACTION : Execute command:sysevent get UPLOAD_LOGS_VAL_DCM");
	    LOGGER.info("STEP 5: EXPECTED : UPLOAD_LOGS_VAL_DCM values should be true ");
	    LOGGER.info("***********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_SYSEVENT_GET, BroadBandTestConstants.VALUE_FOR_UPLOAD_LOGS_VAL_DCM));
	    LOGGER.info("The value is " + response);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.TRUE);
	    if (!status) {
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.SET_VALUE_FOR_UPLOAD_LOGS_VAL_DCM);
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SYSEVENT_GET,
				BroadBandTestConstants.VALUE_FOR_UPLOAD_LOGS_VAL_DCM));
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.TRUE);
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : UPLOAD_LOGS_VAL_DCM values is true");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to enable the \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Enable the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	    LOGGER.info(
		    "STEP 6: ACTION : Exeucte command:curl -X PATCH https://<url:port>/api/v2/device/mac:<MAC>/config -d \"{\"parameters\":[{\"name\":\"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\",\"value\":\"true\",\"dataType\":3}]}\" -H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization: Bearer <SATTOKEN>\"");
	    LOGGER.info(
		    "STEP 6: EXPECTED : \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" parameter value should get set to true with success message");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_CPU_PROC_ANALYZER, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Enabled the \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" parameter");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Process \"cpuprocanalyzer\" is not up";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify the process is up for /usr/bin/cpuprocanalyzer");
	    LOGGER.info("STEP 7: ACTION : Execute command:pidof cpuprocanalyzer");
	    LOGGER.info("STEP 7: EXPECTED : The \"cpuprocanalyzer\" process should be up ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(CommonMethods.getPidOfProcess(device, tapEnv,
		    BroadBandCommandConstants.PROCESS_NAME_CPUPROCANALYZER));

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Process \"cpuprocanalyzer\" is up");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "The default values under /etc/procanalyzerconfig.ini  is not below: FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify the default values under /etc/procanalyzerconfig.ini is FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60  FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600  FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	    LOGGER.info("STEP 8: ACTION : Execute command:cat /etc/procanalyzerconfig.ini");
	    LOGGER.info(
		    "STEP 8: EXPECTED : The default values under /etc/procanalyzerconfig.ini should be FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60  FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600  FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.FEATURE_CPUPROCANALYZER_SLEEP_SECS,
		    BroadBandCommandConstants.FILE_PATH_CPU_PROC_ANALYZER)))
		;
	    errorMessage = "procanalyzerconfig.ini doesn’t contain FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 string";
	    {
		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.FEATURE_CPUPROCANALYZER_TIMETORUN_SECS,
			BroadBandCommandConstants.FILE_PATH_CPU_PROC_ANALYZER)))
		    ;
		errorMessage = "procanalyzerconfig.ini doesn’t contain FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 string";
		{
		    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTestConstants.FEATURE_CPUPROCANALYZER_DYNAMIC,
			    BroadBandCommandConstants.FILE_PATH_CPU_PROC_ANALYZER));
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : The default values under /etc/procanalyzerconfig.ini  is below: FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "The captured data folder cpuprocanalyzer is not under /tmp/cpuprocanalyzer";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Check the captured data folder cpuprocanalyzer is under /tmp/cpuprocanalyzer");
	    LOGGER.info("STEP 9: ACTION : Execute command:ls /tmp/cpuprocanalyzer");
	    LOGGER.info("STEP 9: EXPECTED : The captured data folder cpuprocanalyzer is under /tmp/cpuprocanalyzer");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.doesDirectoryExistInArmConsole(device, tapEnv,
		    BroadBandCommandConstants.FOLDER_PATH_CPU_PROC_ANALYZER,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.TRUE).isStatus();

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : cpuprocanalyzer is available under tmp folder");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = " /tmp/cpuprocanalyzer folder is exceeding 1.5MB";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify /tmp/cpuprocanalyzer folder must not exceed 1.5MB");
	    LOGGER.info("STEP 10: ACTION : Execute command:du -shc /tmp/* | grep -i \"cpuprocanalyzer\"");
	    LOGGER.info("STEP 10: EXPECTED :  /tmp/cpuprocanalyzer folder must not exceed 1.5MB");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.verifyDirectorySizeInArmConsole(device, tapEnv,
		    BroadBandCommandConstants.SIZE_OF_CPUPROCANALYZER, 1.5);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : cpuprocanalyzer size is less than 1.5MB");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "\"Mem Limits curr :\" ,\"Triggering RunCPUProcAnalyzer.sh stop\" and \"Exiting the application\" log strings is not available in  CPUPROCANALYZERlog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify CPUPROCANALYZERlog.txt.0 log is created ");
	    LOGGER.info("STEP 11: ACTION : Execute command:cat /rdklogs/logs/CPUPROCANALYZERlog.txt.0");
	    LOGGER.info(
		    "STEP 11: EXPECTED : \"Mem Limits curr :\" ,\"Triggering RunCPUProcAnalyzer.sh stop\" and \"Exiting the application\" log strings are available in CPUPROCANALYZERlog.txt.0");
	    LOGGER.info("***********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_TRIGGERING_RUNCPUPROCANALYZER,
		    BroadBandCommandConstants.COMMAND_CPUPROCANALYZER_LOG_FILE,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		errorMessage = "CPUPROCANALYZERlog.txt.0 Log doesn’t contain the Triggering RunCPUProcAnalyzer.sh string";
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_EXITING_THE_APPLICATION,
			BroadBandCommandConstants.COMMAND_CPUPROCANALYZER_LOG_FILE,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL :CPUPROCANALYZERlog.txt.0 Log contains the required strings");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Log not uploaded in the Console log.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Veirfy console log on the log upload for the CPAstats");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute command:cat /rdklogs/logs/Consolelog.txt.0 For Atombased device:cat /rdklogs/logs/Armconsolelog.txt.0");
	    LOGGER.info("STEP 12: EXPECTED : The log upload should be uploaded in the Console ");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_CPASTATS)))
		;
	    {
		errorMessage = "Log doesn’t contain the CPASTATS string";
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
			BroadBandTraceConstants.LOG_MESSAGE_LOGS_UPLOADED_SUCCESSFULLY));

	    }
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Log uploaded in the Console log");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Process \"cpuprocanalyzer\" is up";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify the process /usr/bin/cpuprocanalyzer exited after the log run.");
	    LOGGER.info("STEP 13: ACTION : Execute command:pidof cpuprocanalyzer");
	    LOGGER.info("STEP 13: EXPECTED : The \"cpuprocanalyzer\" process should not be up ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(CommonMethods.getPidOfProcess(device, tapEnv,
		    BroadBandCommandConstants.PROCESS_NAME_CPUPROCANALYZER));

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Process \"cpuprocanalyzer\" is not up");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("*******************************************************8***************************");

	    stepNum = "s14";
	    errorMessage = "The  cpuprocanalyzer is present in the folder ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Verify captured data in cpuprocanalyzer is not persistent on post reboot.");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute command:Reboot.Device should come up and check the tmp folder after 10 mins of uptime.");
	    LOGGER.info("STEP 14: EXPECTED : The cpuprocanalyzer should not be present in the folder ");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		errorMessage = "Failed to verify CPUPROCANALYZER after reboot";
		status = !BroadBandCommonUtils.doesDirectoryExistInArmConsole(device, tapEnv,
			BroadBandCommandConstants.FOLDER_PATH_CPU_PROC_ANALYZER,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.TRUE).isStatus();
	    }
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : The  cpuprocanalyzer is not present in the folder");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("***********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CPUPROC-1000");
    }
    
    /**
     * Device SHALL have the ability to retrieve the fan diagnostics via WebPA
     * <li>PRE-CONDITION-1 Get the number of fans installed in the device using TR-181 FanNumberOfEntries</li>
     * <li>PRE-CONDITION-2 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to TRUE using webpa</li>
     * <li>1. Verify the default value of Device.Thermal.Fan.{i}.Status using webpa</li>
     * <li>2. Verify the default value of Device.Thermal.Fan.{i}.Speed using webpa</li>
     * <li>3. Verify the default value of Device.Thermal.Fan.{i}.RotorLock using webpa</li>
     * <li>4. Verify the default value of Device.Thermal.Fan.{i}.MaxOverride using webpa</li>
     * <li>5. Set the value of Device.Thermal.Fan.{i}.MaxOverride to TRUE using webpa</li>
     * <li>6. Verify the value of Device.Thermal.Fan.{i}.MaxOverride using webpa</li>
     * <li>7. Reboot the device</li>
     * <li>8. Verify Device.Thermal.Fan.{i}.Status is not persisted after reboot</li>
     * <li>9. Verify Device.Thermal.Fan.{i}.Speed is not persisted after reboot</li>
     * <li>10. Verify Device.Thermal.Fan.{i}.RotorLock is not persisted after reboot</li>
     * <li>11. Verify the value of Device.Thermal.Fan.{i}.MaxOverride once the device comes online after reboot</li>
     * <li>POST-CONDITION-1 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to False using webpa</li>
     * 
     * @author Dipankar Nalui & Betel Costrow
     * @refactor Said Hisham
     *           </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-THERMAL_FAN-1000")
    public void testToVerifyFanParameters(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-THERMAL_FAN-100";
	String stepNum = null;
	String errorMessage = null;
	String response = null;
	boolean status = false;
	String fanSpeed = null;
	String noOfFanAvailable = null;

	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-THERMAL_FAN-1000");
	LOGGER.info("TEST DESCRIPTION: Device SHALL have the ability to retrieve the fan diagnostics via WebPA");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION-1 Get the number of fans installed in the device using TR-181 FanNumberOfEntries");
	LOGGER.info(
		"PRE-CONDITION-2 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	LOGGER.info("1. Verify the default value of Device.Thermal.Fan.{i}.Status using webpa");
	LOGGER.info("2. Verify the default value of Device.Thermal.Fan.{i}.Speed using webpa");
	LOGGER.info("3. Verify the default value of Device.Thermal.Fan.{i}.RotorLock using webpa");
	LOGGER.info("4. Verify the default value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	LOGGER.info("5. Set the value of Device.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	LOGGER.info("6. Verify the value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	LOGGER.info("7. Reboot the device");
	LOGGER.info("8. Verify Device.Thermal.Fan.{i}.Status is not persisted after reboot ");
	LOGGER.info("9. Verify Device.Thermal.Fan.{i}.Speed is not persisted after reboot ");
	LOGGER.info("10. Verify Device.Thermal.Fan.{i}.RotorLock is not persisted after reboot");
	LOGGER.info(
		"11. Verify the value of Device.Thermal.Fan.{i}.MaxOverride once the device comes online after reboot ");
	LOGGER.info(
		"POST-CONDITION-1 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to False using webpa");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "PRE-CONDITION-1 : DESCRIPTION : Get the number of fans installed in the device using TR-181 FanNumberOfEntries");
	    LOGGER.info("PRE-CONDITION-1 : ACTION : Execute webpa get command: Device.Thermal.FanNumberOfEntries");
	    LOGGER.info(
		    "PRE-CONDITION-1 : EXPECTED : Device.Thermal.FanNumberOfEntries should provide 1 or 2 according to specific devices");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Unable to get no of fan is installed using Device.Thermal.FanNumberOfEntries";
	    noOfFanAvailable = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_NO_ENTRIES);
	    status = CommonMethods.isNotNull(noOfFanAvailable)
		    && !noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);

	    if (status) {
		LOGGER.info("PRE-CONDITION-1 : ACTUAL : Successfully got the number of fans available in device.");
	    } else {
		LOGGER.error("PRE-CONDITION-1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "PRE-CONDITION-2 : DESCRIPTION : Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	    LOGGER.info(
		    "PRE-CONDITION-2 : ACTION : Execute the following command:curl -H \\\"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>device/mac:<MAC>/config -d \\\"{\\\"parameters\\\":[{\\\"dataType\\\":3,\\\"name\\\":\\\"Device.Thermal.Fan.{i}.MaxOverride\\\",\\\"value\\\":\\\"True\\\"}]}\\\"\");\r\n"
			    + "");
	    LOGGER.info(
		    "PRE-CONDITION-2 : EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be set successfully");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to True is not successful";
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("Waiting for 30 seconds to Fan configurations get updated.");

	    if (status) {
		LOGGER.info("PRE-CONDITION-2 : ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value is set successfully");
	    } else {
		LOGGER.error("PRE-CONDITION-2 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    stepNum = "s1";
	    errorMessage = "Device.Thermal.Fan.Status value is not retrieved as TRUE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.Status using webpa");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.Status");
	    LOGGER.info("STEP 1: EXPECTED : Device.Thermal.Fan.{i}.Status value should be retrieved as TRUE");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    BroadBandTestConstants.TRUE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			BroadBandTestConstants.TRUE);
	    }

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Device.Thermal.Fan.{i}.Status value is retrieved as TRUE");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Device.Thermal.Fan.{i}.Speed value  is not  retrieved as an unsigned integer type.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.Speed using webpa");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.Speed");
	    LOGGER.info("STEP 2: EXPECTED : Device.Thermal.Fan.{i}.Speed value should be retrieved");
	    LOGGER.info("**********************************************************************************");

	    fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(fanSpeed)
		    && !fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(fanSpeed)
			&& !fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Device.Thermal.Fan.{i}.Speed value is retrieved");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Device.Thermal.Fan.{i}.RotorLock value  is not  retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify the default value of Device.Thermal.Fan.{i}.RotorLock using webpa");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/v2/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.RotorLock");
	    LOGGER.info("STEP 3: EXPECTED : Device.Thermal.Fan.{i}.RotorLock value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Device.Thermal.Fan.{i}.RotorLock value is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Device.Thermal.Fan.{i}.MaxOverride value  is not retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify the default value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info("STEP 4: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value  is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Device.Thermal.Fan.{i}.MaxOverride value is not set successfully";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Set the value of Device.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":3,\"name\":\"Device.Thermal.Fan.{i}.MaxOverride\",\"value\":\"True\"}]}\"");
	    LOGGER.info("STEP 5: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be set successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value is set successfully");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Device.Thermal.Fan.{i}.MaxOverride value  is not  retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info("STEP 6: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value  is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to reboot and access the device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP 7: ACTION : Execute the following command: /sbin/reboot");
	    LOGGER.info("STEP 7: EXPECTED : Successfully rebooted the device.");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Device.Thermal.Fan.{i}.Status value is not retrieved as False";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify Device.Thermal.Fan.{i}.Status is not persisted after reboot");
	    LOGGER.info("STEP 8: ACTION : Execute webpa get command: Device.Thermal.Fan.{i}.Status");
	    LOGGER.info("STEP 8: EXPECTED : Device.Thermal.Fan.{i}.Status value should be retrieved as False");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Device.Thermal.Fan.{i}.Status value  is not persisted after reboot");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Device.Thermal.Fan.{i}.Speed value is not retrieved as 0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify Device.Thermal.Fan.{i}.Speed is not persisted after reboot");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute webpa get command: Execute webpa get command: Device.Thermal.Fan.{i}.Speed");
	    LOGGER.info("STEP 9: EXPECTED : Device.Thermal.Fan.{i}.Speed value should be retrieved as 0");
	    LOGGER.info("**********************************************************************************");

	    fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(fanSpeed) && fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(fanSpeed)
			&& fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Device.Thermal.Fan.{i}.Speed value is not persisted after reboot");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Device.Thermal.Fan.{i}.RotorLock value is not retrieved as FALSE/Not_Applicable";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify Device.Thermal.Fan.{i}.RotorLock is not persisted after reboot");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute webpa get command: Execute webpa get command: Device.Thermal.Fan.{i}.RotorLock");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Device.Thermal.Fan.{i}.RotorLock value should be retrieved as FALSE/Not_Applicable");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE)
		    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_FAN_NOT_APPLICABLE_ROTOR);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE)
			|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_FAN_NOT_APPLICABLE_ROTOR);
	    }

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Device.Thermal.Fan.{i}.RotorLock value is not persisted after reboot");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Device.Thermal.Fan.MaxOverride Value  is not  retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.MaxOverride once the device comes online after reboot ");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info("STEP 11: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride Value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value  is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    status = false;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "POST-CONDITION-1: DESCRIPTION :Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to False using webpa");
	    LOGGER.info("POST-CONDITION-1: ACTION : Execute webpa set command : Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info(
		    "POST-CONDITION -1: EXPECTED : Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride value should be set successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("POST-CONDITION -1: Device.Thermal.Fan.{i}.MaxOverride value is set to FALSE successfully");
	    } else {
		LOGGER.error(
			"POST-CONDITION -1: Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride value is not set successfully");
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-THERMAL_FAN-1000");
    }
}
